pitcherView <- function(csv, pitcher_name) {
  # csv contains Trackman data
  
  library(plotly)
  library(shiny)
  library(dplyr)
  current_view <- reactiveVal("pitcher")
  
  data <- read.csv(csv, header = TRUE)
  data <- data[(data$Pitcher == pitcher_name), ]
  data <- data[data$PitchCall == "StrikeCalled" | data$PitchCall == "StrikeSwinging", ]
  data <- read.csv(csv, header = TRUE) %>%
    filter(Pitcher == pitcher_name) %>%
    filter(PitchCall %in% c("StrikeCalled", "StrikeSwinging")) %>%
    group_by(TaggedPitchType) %>%
    summarise(
      TaggedPitchType = first(TaggedPitchType),
      RelSpeed = mean(RelSpeed, na.rm = TRUE),
      PitchTrajectoryZc0 = mean(PitchTrajectoryZc0, na.rm = TRUE),
      PitchTrajectoryZc1 = mean(PitchTrajectoryZc1, na.rm = TRUE),
      PitchTrajectoryZc2 = mean(PitchTrajectoryZc2, na.rm = TRUE),
      PitchTrajectoryXc0 = mean(PitchTrajectoryXc0, na.rm = TRUE),
      PitchTrajectoryXc1 = mean(PitchTrajectoryXc1, na.rm = TRUE),
      PitchTrajectoryXc2 = mean(PitchTrajectoryXc2, na.rm = TRUE),
      PitchTrajectoryYc0 = mean(PitchTrajectoryYc0, na.rm = TRUE),
      PitchTrajectoryYc1 = mean(PitchTrajectoryYc1, na.rm = TRUE),
      PitchTrajectoryYc2 = mean(PitchTrajectoryYc2, na.rm = TRUE),
      .groups = "drop"
    )
  
  pitches <- list()
  
  solve_t_final <- function(a, b, c) {
    roots <- polyroot(c(a, b, c))  # solve a*t^2 + b*t + c = 0
    real_roots <- Re(roots[abs(Im(roots)) < 1e-6])
    max(real_roots[real_roots > 0])  # keep only positive root
  }
  
  pitch_colors <- c(
    "Fastball" = "blue",
    "Sinker" = "sienna",
    "Cutter" = "wheat",
    "Splitter" = "turquoise",
    "Slider" = "orange",
    "ChangeUp" = "yellow",
    "Curveball" = "purple",
    "Knuckleball" = "lawngreen",
    "Other" = "slategrey"
  )
  
  default_color <- "slategrey"
  
  pitch_colors <- pitch_colors[names(pitch_colors) %in% data$TaggedPitchType]
  
  distance_to_plate <- 60.5
  mph_to_fps <- 1.467
  
  for (i in seq_len(nrow(data))) {
    t_final <- solve_t_final(
      a = data$PitchTrajectoryYc2[i],
      b = data$PitchTrajectoryYc1[i],
      c = data$PitchTrajectoryYc0[i]
    )
    
    t <- seq(0, t_final, length.out = 50)
    
    pitches[[i]] <- data.frame(
      t = t,
      x = data$PitchTrajectoryZc0[i] + data$PitchTrajectoryZc1[i] * t + data$PitchTrajectoryZc2[i] * t^2,
      y = data$PitchTrajectoryXc0[i] + data$PitchTrajectoryXc1[i] * t + data$PitchTrajectoryXc2[i] * t^2,
      z = data$PitchTrajectoryYc0[i] + data$PitchTrajectoryYc1[i] * t + data$PitchTrajectoryYc2[i] * t^2,
      pitch_type = data$TaggedPitchType[i]
    )
    pitches[[i]] <- pitches[[i]][pitches[[i]]$y >= 0, ]
    
  }
  
  P <- plot_ly()
  for (i in seq_len(nrow(data))) {
    P <- add_trace(P, x = list(NULL), y = list(NULL), z = list(NULL),
                   type = "scatter3d", mode = "markers",
                   marker = list(size = 4, color = "black"),
                   showlegend = FALSE, name = "Ball Marker")
    P <- add_trace(P, x = list(NULL), y = list(NULL), z = list(NULL),
                   type = "scatter3d", mode = "lines",
                   line = list(color = "red", width = 1),
                   showlegend = FALSE, name = "Tail")
  }
  
  P <- add_trace(P, x = c(-0.7, 0.7), y = c(1.4, 1.4), z = c(18 / 12, 18 / 12), type = 'scatter3d', mode = 'lines', line = list(color = 'black', width = 2), showlegend = FALSE, hoverinfo = 'none')
  P <- add_trace(P, x = c(0.7, 0.7), y = c(1.4, 1.4), z = c(18 / 12, 40 / 12), type = 'scatter3d', mode = 'lines', line = list(color = 'black', width = 2), showlegend = FALSE, hoverinfo = 'none')
  P <- add_trace(P, x = c(0.7, -0.7), y = c(1.4, 1.4), z = c(40 / 12, 40 / 12), type = 'scatter3d', mode = 'lines', line = list(color = 'black', width = 2), showlegend = FALSE, hoverinfo = 'none')
  P <- add_trace(P, x = c(-0.7, -0.7), y = c(1.4, 1.4), z = c(40 / 12, 18 / 12), type = 'scatter3d', mode = 'lines', line = list(color = 'black', width = 2), showlegend = FALSE, hoverinfo = 'none')
  
  P <- layout(P,
              annotations = list(
                list(
                  text = pitcher_name,
                  x = 0.5,         # X position (relative to plot range)
                  y = 0.75,        # Y position (a little past mound / plate)
                  z = 100,        # Z position (height above ground)
                  showarrow = FALSE,
                  font = list(size = 16, color = "black"),
                  xanchor = "center",
                  yanchor = "bottom"
                )
              )
  )
  
  # Solid plane at z = 0
  x_plane <- seq(-10, 10, length.out = 50)
  y_plane <- seq(-50, 75, length.out = 50)
  z_plane <- matrix(0, nrow = 50, ncol = 50)  # z = 0 plane (matrix of zeros)
  # Add the plane as a surface
  P <- add_surface(P, x = x_plane, y = y_plane, z = z_plane, opacity = 1, colorscale = list(c(0, 'darkgreen'), c(1, 'darkgreen')), showscale = FALSE, hoverinfo = 'none')
  
  # Pitcher's mound centered at y = 60.5
  mound_center_x <- 0
  mound_center_y <- 60.5
  mound_radius <- 9
  mound_height <- 0.83  # 10 inches
  
  x_mound <- seq(-mound_radius, mound_radius, length.out = 50)
  y_mound <- seq(mound_center_y - mound_radius, mound_center_y + mound_radius, length.out = 50)
  z_mound <- outer(x_mound, y_mound, function(x, y) {
    r <- sqrt((x - mound_center_x)^2 + (y - mound_center_y)^2)
    ifelse(r <= mound_radius, mound_height * (1 - (r / mound_radius)^2), 0)
  })
  
  P <- add_surface(P, x = x_mound, y = y_mound, z = z_mound, opacity = 0.9, colorscale = list(c(0, 'saddlebrown'), c(1, 'saddlebrown')), showscale = FALSE, hoverinfo = 'none')
  
  # Dimensions in feet
  plate_width <- 17 / 12
  plate_depth <- 8.5 / 12
  plate_diagonal <- 8.5 / 12
  plate_x <- c(-plate_width / 2, plate_width / 2, plate_width / 2, 0, -plate_width / 2, -plate_width / 2)
  plate_y <- c(1.4, 1.4, 0, 1.4 - plate_depth, 1.4 - plate_depth, 1.4)
  z_home <- 0.01
  
  
  vertices <- list(
    x = c(-plate_width / 2, plate_width / 2, plate_width / 2, -plate_width / 2, -plate_width / 2),
    y = c(1.4, 1.4, 0.7, 0.7, 1.4),
    z = rep(z_home, 5)
  )
  
  i <- c(0, 0)
  j <- c(1, 2)
  k <- c(2, 3)
  
  # Add to plot
  P <- add_trace(P, type = 'mesh3d', x = vertices$x, y = vertices$y,
                 z = vertices$z, i = i, j = j, k = k, 
                 facecolor = rep("white", 2), opacity = 1,
                 name = 'Home Plate', showlegend = FALSE
  )
  
  
  vertices <- list(
    x = c(plate_width / 2, 0, -plate_width / 2, plate_width / 2),
    y = c(0.7, 0, 0.7, 0.7),
    z = rep(z_home, 4)
  )
  
  i <- c(0, 0)
  j <- c(1, 2)
  k <- c(2, 3)
  
  # Add to plot
  P <- add_trace(P, type = 'mesh3d', x = vertices$x, y = vertices$y,
                 z = vertices$z, i = i, j = j, k = k, 
                 facecolor = rep("white", 2), opacity = 1,
                 name = 'Home Plate', showlegend = FALSE
  )
  
  # Circle of dirt around home plate
  theta <- seq(0, 2 * pi, length.out = 100)
  circle_radius <- 13
  x_circle <- circle_radius * cos(theta)
  y_circle <- circle_radius * sin(theta) + 1.4  # center at home plate (y = 1.4)
  z_circle <- rep(0, length(theta))
  
  # Add center point
  x_mesh <- c(0, x_circle)
  y_mesh <- c(1.4, y_circle)
  z_mesh <- c(0.01, z_circle)
  
  i <- rep(0, length(theta) - 1)
  j <- 1:(length(theta) - 1)
  k <- 2:length(theta)
  k[length(k)] <- 1
  
  P <- add_trace(P, type = "mesh3d", x = x_mesh, y = y_mesh, z = z_mesh,
                 i = i, j = j, k = k, facecolor = rep("saddlebrown", length(i)),
                 showscale = FALSE, opacity = 1, showlegend = FALSE, hoverinfo = "none")
  
  # Dimensions
  box_length <- 4       # front-to-back (x-axis)
  box_width <- 6       # side-to-side (y-axis)
  gap <- 0.5            # distance from plate to inner edge of box
  z <- 0            # height just above ground
  
  # Home Plate dimensions
  plate_width <- 17 / 12
  plate_depth <- 17 / 12
  z_home <- 0.01  # Just above the ground
  
  
  # Right-handed batter's box (on the left side of plate, from pitcher's perspective)
  right_x <- c(plate_x[1] - gap, plate_x[1] - gap, plate_x[1] - gap - box_length, plate_x[1] - gap - box_length, plate_x[1] - gap, plate_x[1] - gap)
  right_y <- c(plate_y[1], plate_y[2], plate_y[2], plate_y[3] - box_width, plate_y[4] - box_width, plate_y[1])
  
  # Left-handed batter's box (on the right side of plate, from pitcher's perspective)
  left_x <- c(plate_x[2] + gap, plate_x[2] + gap, plate_x[2] + gap + box_length, plate_x[2] + gap + box_length, plate_x[2] + gap, plate_x[2] + gap)
  left_y <- c(plate_y[1], plate_y[2], plate_y[2], plate_y[3] - box_width, plate_y[4] - box_width, plate_y[1])
  
  # Plotting
  P <- P %>%
    add_trace(type = "scatter3d", mode = "lines",
              x = right_x, y = right_y, z = rep(z, length(right_x)),
              line = list(color = "white", width = 30),
              showlegend = FALSE) %>%
    add_trace(type = "scatter3d", mode = "lines",
              x = left_x, y = left_y, z = rep(z, length(left_x)),
              line = list(color = "white", width = 30),
              showlegend = FALSE)
  
  # Dimensions in feet
  rubber_width <- 2
  rubber_depth <- 2
  z_rubber <- 0.83  # Just above the ground
  
  # Center position of the rubber
  x_center <- 0
  y_center <- 60.5
  
  vertices <- list(
    x = c(x_center - rubber_width/2, x_center + rubber_width/2,
          x_center + rubber_width/2, x_center - rubber_width/2),
    y = c(y_center - rubber_depth/2, y_center - rubber_depth/2,
          y_center + rubber_depth/2, y_center + rubber_depth/2),
    z = rep(z_rubber, 4)
  )
  
  i <- c(0, 0)
  j <- c(1, 2)
  k <- c(2, 3)
  
  # Add to plot
  P <- add_trace(P, type = 'mesh3d',
                 x = vertices$x, y = vertices$y, z = vertices$z,
                 i = i, j = j, k = k,
                 facecolor = rep("white", 2),
                 opacity = 1,
                 name = 'Pitching Rubber', showlegend = FALSE)
  
  
  P <- layout(P, scene = list(
    camera = list(eye = list(x = 0, y = 0.55, z = 0.058), 
                  center = list(x = 0, y = 0.1, z = -0.05),
                  up = list(x = 0, y = 0, z = 1)),
    xaxis = list(title = NULL, range = c(-10, 10), showgrid = FALSE, zeroline = FALSE, ticktext = NULL, tickvals = NULL, showspikes = FALSE),
    yaxis = list(title = NULL, range = c(0, 60.5), showgrid = FALSE, zeroline = FALSE, ticktext = NULL, tickvals = NULL, showspikes = FALSE),
    zaxis = list(title = NULL, range = c(-10, 20), showgrid = FALSE, zeroline = FALSE, ticktext = NULL, tickvals = NULL, showspikes = FALSE),
    aspectmode = "manual",
    aspectratio = list(x = 1, y = 1, z = 1)))
  
  camera_view <- list(
    camera = list(eye = list(x = 0, y = 0.55, z = 0.058), 
                  center = list(x = 0, y = 0.1, z = -0.05),
                  up = list(x = 0, y = 0, z = 1)),
    xaxis = list(title = NULL, range = c(-10, 10), showgrid = FALSE, zeroline = FALSE, ticktext = NULL, tickvals = NULL, showspikes = FALSE),
    yaxis = list(title = NULL, range = c(0, 60.5), showgrid = FALSE, zeroline = FALSE, ticktext = NULL, tickvals = NULL, showspikes = FALSE),
    zaxis = list(title = NULL, range = c(-10, 20), showgrid = FALSE, zeroline = FALSE, ticktext = NULL, tickvals = NULL, showspikes = FALSE),
    aspectmode = "manual",
    aspectratio = list(x = 1, y = 1, z = 1))
  
  fixed_camera <- list(
    eye = list(x = 0, y = 0.9, z = 0.058), 
    center = list(x = 0, y = 0, z = 0),
    up = list(x = 0, y = 0, z = 1)
  )
  
  
  pitch <- pitches[[1]]
  
  ui <- fluidPage(
    actionButton("start", "Animate Pitches"),
    plotlyOutput("pitchPlot")
  )
  
  
  server <- function(input, output, session) {
    
    for (pitch_type in names(pitch_colors)) {
      P <- add_trace(P,
                     x = list(NULL), y = list(NULL), z = list(NULL),
                     type = "scatter3d", mode = "markers",
                     marker = list(size = 8, color = pitch_colors[[pitch_type]]),
                     name = pitch_type,
                     showlegend = TRUE)
    }
    
    # Initial plot
    output$pitchPlot <- renderPlotly({
      P %>%
        layout(
          scene = list(camera = fixed_camera),
          uirevision = TRUE
        )
    })
    
    
    
    observeEvent(input$start, {
      trail_length <- 5
      
      for (i in seq_along(pitches)) {
        pitch <- pitches[[i]]
        pitch_type <- data$TaggedPitchType[i]
        color <- pitch_colors[[pitch_type]]
        if (is.null(color)) color <- default_color
        
        for (j in seq_len(nrow(pitch))) {
          start_idx <- max(1, j - trail_length + 1)
          
          # Update tail
          plotlyProxy("pitchPlot", session) %>%
            plotlyProxyInvoke("restyle", list(
              x = list(list(pitch$x[start_idx:j])),
              y = list(list(pitch$y[start_idx:j])),
              z = list(list(pitch$z[start_idx:j]))
            ), list(1))
          
          # Update animated ball
          plotlyProxy("pitchPlot", session) %>%
            plotlyProxyInvoke("restyle", list(
              x = list(list(pitch$x[j])),
              y = list(list(pitch$y[j])),
              z = list(list(pitch$z[j])),
              marker = list(list(size = 6, color = color))
            ), list(2))
          
          Sys.sleep(0.02)
        }
        
        # After animation, add full trace and final dot
        plotlyProxy("pitchPlot", session) %>%
          plotlyProxyInvoke("addTraces", list(
            list(
              x = pitch$x,
              y = pitch$y,
              z = pitch$z,
              type = "scatter3d",
              mode = "lines",
              line = list(color = color, width = 2, opacity = 0.3),
              showlegend = FALSE
            ),
            list(
              x = list(tail(pitch$x, 1)),
              y = list(tail(pitch$y, 1)),
              z = list(tail(pitch$z, 1)),
              type = "scatter3d",
              mode = "markers",
              marker = list(size = 6, color = color),
              name = pitch_type,
              sizemode = "diameter",
              showlegend = FALSE
            )
          ))
      }
    })
  }
  
  
  # First frame (initialize with only the first point)
  P_anim <- P
  for (j in seq_along(pitches)) {
    pitch_j <- pitches[[j]]
    
    # Initial trail (empty)
    P_anim <- add_trace(P_anim,
                        x = pitch_j$x[1],
                        y = pitch_j$y[1],
                        z = pitch_j$z[1],
                        type = "scatter3d", mode = "lines",
                        line = list(color = "red", width = 1),
                        showlegend = FALSE)
    
    # Initial ball marker
    P_anim <- add_trace(P_anim,
                        x = pitch_j$x[1],
                        y = pitch_j$y[1],
                        z = pitch_j$z[1],
                        type = "scatter3d", mode = "markers",
                        marker = list(size = 6, color = "blue"),
                        showlegend = FALSE)
  }
  
  
  # Maximum frame count
  frame_count <- max(sapply(pitches, nrow))
  
  # Create frames where each pitch is drawn up to time i
  frames <- lapply(seq_len(nrow(pitches[[1]])), function(i) {
    frame_data <- list()
    
    for (j in seq_along(pitches)) {
      pitch_j <- pitches[[j]]
      
      frame_data[[length(frame_data) + 1]] <- list(
        type = "scatter3d",
        mode = "lines",
        x = as.list(pitch_j$x[1:i]),
        y = as.list(pitch_j$y[1:i]),
        z = as.list(pitch_j$z[1:i]),
        line = list(color = "red", width = 2),
        showlegend = FALSE
      )
      
      # Ball (marker)
      frame_data[[length(frame_data) + 1]] <- list(
        type = "scatter3d",
        mode = "markers",
        x = list(pitch_j$x[i]),
        y = list(pitch_j$y[i]),
        z = list(pitch_j$z[i]),
        marker = list(size = 6, color = "blue"),
        showlegend = FALSE
      )
    }
    
    list(name = as.character(i), data = frame_data, traces = seq_along(frame_data))
  })
  
  
  
  # Animation layout
  P_anim <- P_anim %>%
    layout(
      updatemenus = list(
        list(
          type = "buttons",
          showactive = FALSE,
          y = 1,
          x = 0.1,
          buttons = list(
            list(
              label = "▶ Play",
              method = "animate",
              args = list(
                NULL,
                list(frame = list(duration = 5, redraw = TRUE),
                     mode = "immediate",
                     transition = list(duration = 0))
              )
            ),
            list(
              label = "❚❚ Pause",
              method = "animate",
              args = list(
                list(NULL),
                list(mode = "immediate",
                     transition = list(duration = 0))
              )
            )
          )
        )
      ),
      sliders = list(
        list(
          active = 0,
          steps = lapply(seq_len(frame_count), function(i) {
            list(label = as.character(i), method = "animate", args = list(list(as.character(i)), list(mode = "immediate")))
          })
        )
      )
    )
  
  
  # Attach frames
  P_anim$x$frames <- frames
  
  # Show final animated plot
  P_anim
  
  shinyApp(ui, server)
  
  
  # X: horizontal movement (- towards right handed batter, + towards lefty)
  # Y: distance from home plate towards pitchers mound
  # Z: height of the pitch off the ground
}
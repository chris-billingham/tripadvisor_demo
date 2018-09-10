# docker utility functions

# start a docker, with switches
# list all docker images and look for one, if not download

docker_images <- function() {
  docker <- Sys.which("docker")
  # get the docker output
  images <- system2(command = docker, args = c("images"), stdout = TRUE)
  
  # divvy up the columns
  columns <- images[1]
  
  # find front and back
  repos_loc <- stringr::str_locate(columns, "REPOSITORY")
  tag_loc <- stringr::str_locate(columns, "TAG")
  image_id_loc <- stringr::str_locate(columns, "IMAGE ID")
  created_loc <- stringr::str_locate(columns, "CREATED")
  size_loc <- stringr::str_locate(columns, "SIZE")
  
  # helper function
  split_and_place <- function(char) {
    repos <- trimws(substr(char, repos_loc[1], tag_loc[1]-1))
    tag <- trimws(substr(char, tag_loc[1], image_id_loc[1]-1))
    image_id <- trimws(substr(char, image_id_loc[1], created_loc[1]-1))
    created <- trimws(substr(char, created_loc[1], size_loc[1]-1))
    size <- trimws(substr(char, size_loc[1], nchar(char)))
    
    df <- data.frame(repo = repos,
                     tag = tag,
                     image_id = image_id,
                     created = created,
                     size = size,
                     stringsAsFactors = FALSE)
    
    return(df)
  }
  
  # mash together
  images_df <- purrr::map_df(images[-1], split_and_place)
  return(images_df)
}

# running docker containers
docker_containers_running <- function() {
  docker <- Sys.which("docker")
  # get the docker output
  containers <- system2(command = docker, args = c("container ls"), stdout = TRUE)
  
  # divvy up the columns
  columns <- containers[1]
  
  # find front and back
  container_loc <- stringr::str_locate(columns, "CONTAINER ID")
  image_loc <- stringr::str_locate(columns, "IMAGE")
  command_loc <- stringr::str_locate(columns, "COMMAND")
  created_loc <- stringr::str_locate(columns, "CREATED")
  status_loc <- stringr::str_locate(columns, "STATUS")
  ports_loc <- stringr::str_locate(columns, "PORTS")
  names_loc <- stringr::str_locate(columns, "NAMES")
  
  # helper function
  split_and_place <- function(char) {
    container <- trimws(substr(char, container_loc[1], image_loc[1]-1))
    image <- trimws(substr(char, image_loc[1], command_loc[1]-1))
    command <- trimws(substr(char, command_loc[1], created_loc[1]-1))
    created <- trimws(substr(char, created_loc[1], status_loc[1]-1))
    status <- trimws(substr(char, status_loc[1], ports_loc[1]-1))
    ports <- trimws(substr(char, ports_loc[1], names_loc[1]-1))
    names <- trimws(substr(char, names_loc[1], nchar(char)))
    
    df <- data.frame(container = container,
                     image = image,
                     command = command,
                     created = created,
                     status = status,
                     ports = ports,
                     names = names,
                     stringsAsFactors = FALSE)
    
    return(df)
  }
  # mash together
  containers_df <- purrr::map_df(containers[-1], split_and_place)
  return(containers_df)
}

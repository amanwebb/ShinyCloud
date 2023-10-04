# Use the official R base image
FROM rocker/r-ver:4.1.0

# Install any required R packages
RUN R -e "install.packages('shiny')"

# Copy your Shiny app code into the container
COPY . /app

# Expose port 80
EXPOSE 80

# Run your Shiny app when the container starts
CMD ["R", "-e", "shiny::runApp('/app')"]

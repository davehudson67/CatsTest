# Base image with R and Shiny
FROM rocker/shiny-verse:4.3.1

# Create and set the working directory
RUN mkdir /home/shiny-app
WORKDIR /home/shiny-app

# Install necessary system dependencies for geospatial R packages
RUN apt-get update && apt-get install -y \
    texlive-latex-base \
    texlive-fonts-recommended \
    texlive-fonts-extra \
    texlive-latex-extra \
    texlive-xetex \
    texlive-lang-all \
    pandoc \
    libproj-dev \
    libgdal-dev \
    libudunits2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install renv package
RUN R -e 'install.packages("renv")'

# Set permissions and user
RUN chown -R shiny:shiny .
USER shiny

# Initialize renv (optional: typically already done locally)
RUN R -e 'renv::init()'

# Copy renv.lock file for reproducibility
COPY renv.lock renv.lock

# Restore R packages from renv.lock
RUN R -e 'renv::restore()'

# Copy the Shiny app files
COPY app/ app/

# Expose the desired port for the Shiny app
EXPOSE 12377

# Start the Shiny app
ENTRYPOINT [ "R", "-e", "shiny::runApp('./app/app.R', port = 12377, host = '0.0.0.0')" ]


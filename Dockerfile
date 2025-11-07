# Usa un'immagine Ubuntu come base
FROM ubuntu:latest

# Imposta l'installer in modalità non interattiva
ENV DEBIAN_FRONTEND=noninteractive

# Installa dipendenze di sistema: R, compilatori (build-essential) e librerie necessarie (zlib1g-dev, pkg-config)
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    software-properties-common \
    dirmngr \
    wget \
    build-essential \
    zlib1g-dev \
    pkg-config \
    pandoc && \
    rm -rf /var/lib/apt/lists/*

# Aggiunge il repository CRAN e installa R
RUN wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc && \
    add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/" && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
    r-base \
    r-base-dev && \
    rm -rf /var/lib/apt/lists/*

# Installa pip (gestore pacchetti Python) e JupyterLab
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    python3-pip && \
    pip install --break-system-packages jupyterlab && \
    rm -rf /var/lib/apt/lists/*

# Installa il kernel R per Jupyter (IRkernel) e i pacchetti R richiesti (data.table)
RUN R -e "install.packages(c('data.table', 'IRkernel', 'rmarkdown', 'knitr'), repos='https://cloud.r-project.org')" && \
    R -e "IRkernel::installspec(name = 'irkernel', displayname = 'R')"

# Porta su cui JupyterLab verrà eseguito
EXPOSE 8888

# Imposta la directory di lavoro all'interno del container
WORKDIR /home/project

# Comando per avviare JupyterLab (senza browser e senza token/password per facilità d'uso)
CMD ["jupyter-lab", "--ip=0.0.0.0", "--port=8888", "--no-browser", "--allow-root", "--ServerApp.token=''", "--ServerApp.password=''"]
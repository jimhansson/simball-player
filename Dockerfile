# Dockerfile för Simball Player API (Common Lisp, SBCL, Clack)
FROM ubuntu:24.04

# Installera beroenden
RUN apt-get update && \
    apt-get install -y sbcl curl git && \
    rm -rf /var/lib/apt/lists/*

# Installera Quicklisp
WORKDIR /opt
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(quit)'

# Lägg till Quicklisp i SBCL:s init-fil
RUN echo "(load \"/root/quicklisp/setup.lisp\")" >> /root/.sbclrc

# Kopiera in projektet
WORKDIR /app
COPY . /app

# Installera projektberoenden
RUN sbcl --eval "(ql:quickload :simball-player)" --eval "(quit)"

# Exponera porten
EXPOSE 5000

# Startkommando
CMD ["sbcl", "--eval", "(ql:quickload :simball-player)", "--eval", "(simball-player:start-server :port 5000)"]

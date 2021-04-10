FROM lambci/lambda:build-provided

RUN curl -sSL https://get.haskellstack.org/ | sh
RUN mkdir /app/
ADD src/ /app/
ADD Setup.hs package.yaml stack.yaml stack.yaml.lock /app/
WORKDIR /app/
RUN stack build --ghc-options='-optl-static'

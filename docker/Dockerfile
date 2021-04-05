FROM lambci/lambda:build-provided

RUN curl -sSL https://download.fpcomplete.com/centos/7/fpco.repo | sudo tee /etc/yum.repos.d/fpco.repo
RUN yum install stack
RUN stack build

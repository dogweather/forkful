---
title:                "Iniciando um novo projeto"
html_title:           "Javascript: Iniciando um novo projeto"
simple_title:         "Iniciando um novo projeto"
programming_language: "Python"
category:             "Python"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Começando um Novo Projeto em Python

## O Que & Por Quê?

Iniciar um novo projeto de programação é como dar o primeiro passo numa jornada emocionante de criar algo novo e útil a partir do zero. Programadores fazem isso para resolver problemas, automatizar tarefas monótonas, aprender novos conceitos e tecnologias, ou simplesmente por diversão e satisfação pessoal.

## Como Fazer:

Em Python, um projeto poderia começar com a criação de um novo diretório e um ambiente virtual para gerir as dependências do projeto. Aqui está um exemplo:

```Python 
# Criar um novo diretório para o projeto
import os
os.mkdir('meu_projeto')

# Navegar para o novo diretório
os.chdir('meu_projeto')

# Criar um ambiente virtual no diretório
os.system('python3 -m venv venv')
```

Execução disso vai criar um novo diretório chamado 'meu_projeto' e um ambiente virtual nele.

## Mergulho Profundo:

Historicamente, iniciar um novo projeto de programação significava simplesmente criar um novo arquivo de código. No entanto, à medida que os projetos se tornam mais complexos e as colaborações mais comuns, os programadores adotaram estruturas mais organizadas para seus projetos.

Existem alternativas para começar um novo projeto em Python. Uma alternativa popular é usar um ambiente de desenvolvimento integrado (IDE) que gere a estrutura do projeto para você. Outra é usar ferramentas de linha de comando, como Cookiecutter, que criam uma estrutura de projeto a partir de um modelo predefinido.

Em relação aos detalhes de implementação, uma parte chave é a criação de um ambiente virtual. Isso isola as dependências do seu projeto do resto do seu sistema, prevenindo conflitos de versões e permitindo a reprodutibilidade do seu código em outras máquinas.

## Veja Também:

1. Documentação oficial do Python para venv: [https://docs.python.org/3/library/venv.html](https://docs.python.org/3/library/venv.html)
2. Site oficial do Cookiecutter: [https://cookiecutter.readthedocs.io](https://cookiecutter.readthedocs.io)
3. Tutorial para configuração do ambiente de desenvolvimento Python: [https://realpython.com/tutorials/development-environments/](https://realpython.com/tutorials/development-environments/)
---
title:                "Iniciando um novo projeto"
html_title:           "Javascript: Iniciando um novo projeto"
simple_title:         "Iniciando um novo projeto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Começando um novo projeto com a Bash: Guia Rápido e Direto

## O Que & Por Quê?
Começar um novo projeto é a fase onde você prepara o ambiente e cria a base para seu código. Aqui, programadores estabelecem uma fundação sólida e organizada para trabalhar de forma eficaz e eficiente.

## Como Fazer:
Bash é bem direto para começar um novo projeto. Eis alguns comandos que você pode achar útil:

```Bash
# Criar uma nova pasta para o seu projeto
mkdir MeuProjeto

# Acessar a pasta do projeto
cd MeuProjeto

# Criar um novo arquivo de Bash
touch novoScript.sh

# Adicionar permissão de execução para o arquivo
chmod +x novoScript.sh

# Escreva seu Bash script com o editor de texto de sua preferência
nano novoScript.sh
```

O conteúdo do `novoScript.sh` pode ser algo como:

```Bash
#!/bin/bash
echo "Olá, Mundo!"
```

E aqui está a saída que você verá:

```Bash
./novoScript.sh
Olá, Mundo!
```

## Mergulhando Fundo
Bash, ou Bourne Again SHell, é um interpretador de linha de comando lançado em 1989. Ele é uma expansão do shell Bourne original, adicionando funcionalidades úteis, como programação de script.

Quanto às alternativas, existem muitas outras shells, como Zsh, Fish, e mais recentemente, o PowerShell da Microsoft que está cross-platform. Entretanto, Bash ainda é uma das mais populares devido à sua onipresença nos sistemas *nix.

Quando você cria um novo projeto com Bash, está, na verdade, criando um ambiente que pode ser escrito e executado em uma linguagem de script. A vantagem disso é que você tem controle direto sobre o sistema operacional, permitindo que faça coisas como mover arquivos ou realizar tarefas de backup automaticamente.

## Veja Também
1. Bash Scripting Tutorial for Beginners: https://www.learnshell.org/
2. Guia de Bash Scripting do Google: https://google.github.io/styleguide/shellguide.html
3. Site oficial do Bash: https://www.gnu.org/software/bash/
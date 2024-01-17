---
title:                "Começando um novo projeto"
html_title:           "Ruby: Começando um novo projeto"
simple_title:         "Começando um novo projeto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

## O que e por que?

Comecando um novo projeto em programacao significa iniciar um novo trabalho com um objetivo especifico em mente, como a criacao de um novo aplicativo ou a resolucao de um problema. Os programadores fazem isso para explorar ideias e tecnologias, ampliar suas habilidades e criar algo novo que possa ser usado por eles ou por outras pessoas.

## Como fazer:

Para criar um novo projeto em Ruby, siga estes passos simples:

```Ruby
# 1. Crie um novo diretorio para o seu projeto
mkdir novo_projeto

# 2. Entre no diretorio recem-criado
cd novo_projeto

# 3. Inicie um novo projeto usando o gem bundler
bundle init
```

Apos seguir esses passos, voce tera um novo projeto criado com um Gemfile padrao.

## Profundando na questao:

Historicamente, a criacao de novos projetos em Ruby era feita manualmente, criando uma estrutura de pasta e instalando manualmente as dependencias desejadas. No entanto, com o surgimento do bundler, isso se tornou muito mais facil e padronizado. Alternativas ao bundler incluem o RubyGems e o Rake.

Para personalizar ainda mais o processo de criacao de um novo projeto, voce pode especificar as dependencias necessarias em seu Gemfile antes de executar o comando `bundle init`. Isso garantira que todas as dependencias necessarias sejam instaladas automaticamente ao iniciar um novo projeto.

## Veja tambem:

- [RubyGems](https://rubygems.org/)
- [Rake](https://ruby.github.io/rake/)
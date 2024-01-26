---
title:                "Iniciando um novo projeto"
date:                  2024-01-20T18:03:36.552613-07:00
model:                 gpt-4-1106-preview
simple_title:         "Iniciando um novo projeto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Começar um novo projeto em Haskell envolve criar a estrutura base de um programa, definindo seus módulos e dependências. Programadores fazem isso para organizar suas ideias e garantir que todos os componentes necessários sejam considerados desde o início.

## Como fazer:

Para iniciar um projeto Haskell, você precisa do Stack, um gerenciador de sistema de compilação e projetos. Aqui está como você configura um projeto básico:

```Haskell
-- Instalar o Stack (se ainda não estiver instalado)
$ curl -sSL https://get.haskellstack.org/ | sh

-- Criar um novo projeto usando Stack
$ stack new meu_projeto

-- Entrar no diretório do projeto
$ cd meu_projeto

-- Construir o projeto (pode demorar um pouco na primeira vez)
$ stack build

-- Executar o projeto
$ stack exec meu_projeto-exe
```

Após executar esses comandos, você terá um novo projeto Haskell com uma estrutura padrão pronta para desenvolver seu aplicativo.

## Mergulho Profundo:

Haskell evoluiu muito desde quando foi lançado em 1990. Iniciar um novo projeto, outrora uma tarefa manual e propensa a erros, tornou-se mais fácil com ferramentas como o Stack mencionado anteriormente. Alternativas ao Stack incluem o Cabal, uma ferramenta mais antiga que ainda é muito utilizada. No nível de implementação, começar um novo projeto com Stack oferece uma série de vantagens, como gestão de dependências confiável, configuração simplificada e compatibilidade com vários compiladores GHC (Glasgow Haskell Compiler). O Stack utiliza um arquivo de manifesto chamado `stack.yaml`, que define o ambiente de construção do projeto.

## Veja Também:

- Documentação do Haskell Stack: [https://docs.haskellstack.org/en/stable/README/](https://docs.haskellstack.org/en/stable/README/)
- Guia para iniciantes em Haskell: [http://learnyouahaskell.com/](http://learnyouahaskell.com/)
- Referência da The Haskell Tool Stack: [https://www.haskell.org/cabal/users-guide/nix-local-build-overview.html](https://www.haskell.org/cabal/users-guide/nix-local-build-overview.html)
- Haskell Package Hackage: [https://hackage.haskell.org/](https://hackage.haskell.org/)
- GHC: [https://www.haskell.org/ghc/](https://www.haskell.org/ghc/)

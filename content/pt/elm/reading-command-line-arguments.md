---
title:                "Lendo argumentos da linha de comando"
html_title:           "Elm: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Ler argumentos de linha de comando é uma técnica essencial para os programadores em Elm. Isso permite que o programa obtenha informações do usuário através da linha de comando, tornando-o mais interativo e versátil.

## Como:

Usando a função ```Elm CommandLine.builtin```, podemos ler os argumentos da linha de comando e armazená-los em uma lista de strings. Por exemplo:

```
Elm CommandLine.builtin
-- Utilizando o comando "run", execute o programa com argumentos
run 1 2 3
-- Output: ["1", "2", "3"]
```

## Detalhes:

Ler argumentos de linha de comando é uma funcionalidade comum em muitas linguagens de programação, incluindo Elm. Essa técnica permite que os programas sejam mais interativos, pois podem receber informações do usuário em tempo de execução. Alternativas para a leitura de argumentos incluem a entrada de dados por meio de um prompt ou através de um arquivo externo.

A função ```Elm CommandLine.builtin``` usa uma biblioteca conhecida como "System Exec Extra" que fornece uma interface para o sistema de arquivos e processo de entrada/saída em Elm. Isso significa que os argumentos da linha de comando são lidos diretamente do sistema operacional, tornando o processo rápido e eficiente.

## Veja também:

- Documentação oficial do Elm: https://elm-lang.org/
- Sistema Exec Extra: https://package.elm-lang.org/packages/elm/core/latest/
- Tutorial sobre entrada/saída em Elm: https://guide.elm-lang.org/interop/
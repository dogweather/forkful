---
title:                "Extraindo subtrings"
html_title:           "Fish Shell: Extraindo subtrings"
simple_title:         "Extraindo subtrings"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Extrair substrings é um processo de separação de uma string maior em partes menores. Isso é comumente usado pelos programadores para obter partes específicas de uma string, como uma palavra ou um caractere específico.

Os programadores usam a extração de substrings para manipular e analisar dados de forma mais eficiente, permitindo que eles obtenham apenas a informação necessária para suas tarefas.

## Como fazer:

```Fish Shell
# Extrair caracteres específicos
set string "Olá mundo"
echo $string[2]

# Resultado: "á"

# Extrair palavras específicas
set string "Eu amo programar em Fish Shell"
echo $string[4..7]

# Resultado: "programar em Fish"

# Extrair partes de uma URL
set url "https://github.com/nathaliairodrigues?tab=repositories"
echo $url[8..21]

# Resultado: "github.com"

```

## Mergulho Profundo:

A extração de substrings tem sido uma técnica essencial desde os primeiros dias da programação de computadores. Anteriormente, era feita manualmente usando funções específicas de cada linguagem de programação. No entanto, com o avanço da tecnologia, as linguagens de programação modernas, como Fish Shell, possuem funções integradas para facilitar a extração de substrings.

Algumas alternativas para a extração de substrings incluem o uso de expressões regulares ou de recursos específicos da linguagem, como o método "slice" em Python.

A implementação da função de extração de substrings em Fish Shell é baseada no indexamento de strings, que é a capacidade de acessar caracteres específicos em uma string usando sua posição numérica. Isso significa que o primeiro caractere em uma string tem o índice 1, o segundo tem o índice 2 e assim por diante.

## Veja também:

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial de extração de substrings em Fish Shell](https://dev.to/nathaliairodrigues/how-to-extract-substrings-in-fish-shell-1kom)
---
title:                "Concatenando strings"
html_title:           "Fish Shell: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## O que e por que?

Concatenação de strings é simplesmente unir duas ou mais strings em uma única string. Programadores geralmente fazem isso quando precisam combinar informações diferentes para formar uma única mensagem ou para criar nomes dinâmicos para variáveis e funções.

## Como fazer:

Usando o Fish Shell, a concatenação de strings é bastante fácil. Basta utilizar o sinal de adição (+) entre as strings que você deseja juntar. Veja o exemplo abaixo:

```
Fish Shell> set operacao "adição"
Fish Shell> echo "Operação escolhida:" + $operacao
Operação escolhida: adição
```

Note que a variável `$operacao` foi concatenada com a string fixa "Operação escolhida: " para formar a mensagem final.

## Mergulho profundo:

A concatenação de strings é uma técnica muito utilizada em diversas linguagens de programação, pois permite a criação de mensagens personalizadas e dinâmicas. Alternativas incluem o uso de placeholders ou a utilização de funções específicas da linguagem para formatação de strings. O processo de concatenação geralmente envolve a alocação de um novo espaço de memória para armazenar a string resultante.

## Veja também:

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/commands.html#set)
- [Definição de Concatenação de Strings na Wikipedia](https://pt.wikipedia.org/wiki/Concatena%C3%A7%C3%A3o)
- [Artigo sobre a importância da concatenação de strings em programação](https://www.educba.com/string-concatenation-in-programming/)
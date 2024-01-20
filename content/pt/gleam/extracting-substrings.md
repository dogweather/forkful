---
title:                "Extraindo substrings"
html_title:           "Bash: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## O que é e por quê?

Extrair substrings é o processo de obter uma parte de uma cadeia de caracteres. Programadores fazem isso para manipular e analisar dados, isolando partes importantes de um conjunto maior de dados.

## Como fazer:

Aqui está um exemplo básico de como extrair uma substring em Gleam:

```gleam
import gleam/string

string.slice("Olá, Mundo!", 0, 4)
```

O código acima retornará `Olá`.

## Imersão profunda:

1. Historia: Embora Gleam seja uma linguagem relativamente nova, a extração de substrings é uma funcionalidade básica encontrada na maioria das linguagens de programação.
2. Alternativas: No Gleam, podemos também usar a função `substring` para obter o mesmo resultado.
3. Detalhes de implementação: Em Gleam, `string.slice` pega três argumentos: a string original, o início da substring e o final da substring.

## Ver também:

Para mais informações sobre a manipulação de strings em Gleam, você pode verificar os seguintes links:

1. Documentação oficial de Gleam para a Função String: [Clique aqui](https://gleam.run/book/tour/strings.html)
2. Tutorial de YouTube sobre a linguagem de programação Gleam: [Clique aqui](https://www.youtube.com/watch?v=R2FtPcXkYEo)
3. Curso online gratuito sobre principais funções de string no Gleam: [Clique aqui](https://www.learn-gleam-free.com/string-functions)
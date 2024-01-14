---
title:                "Gleam: Maiúsculas em uma string"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que?

Às vezes, em programação, precisamos manipular strings de diversas maneiras. Uma dessas maneiras é capitalizar uma string, ou seja, transformar todas as letras em maiúsculas. Mas por que alguém faria isso? Bem, dependendo da aplicação que está sendo desenvolvida, pode ser necessário garantir que as letras sejam consistentes em toda a string, ou simplesmente por preferência estética.

## Como fazer:

Vamos ver como capitalizar uma string em Gleam. Primeiro, precisamos importar o módulo "String" e, em seguida, usar a função "to_uppercase" que recebe a string que queremos capitalizar como parâmetro:

```Gleam
import String

String.to_uppercase("ola mundo")   # Retorna "OLA MUNDO"
```

Como mostrado no exemplo acima, toda a string "ola mundo" foi transformada em letras maiúsculas.

## Profundidade:

Mas como isso funciona por trás dos panos? Basicamente, a função "to_uppercase" itera sobre cada caractere da string e, se for uma letra minúscula, a transforma em maiúscula. Se não for uma letra, retorna o próprio caractere. Por exemplo, "ola mundo" se tornaria "OLA MUNDO". Além disso, é importante saber que a função retorna uma nova string, ou seja, a string original não é modificada.

## Veja também:

- [Documentação oficial do módulo String](https://gleam.run/modules/string/)
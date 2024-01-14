---
title:    "Gleam: Convertendo uma string para minúsculas"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que

Se você está trabalhando com strings em Gleam, pode ser útil saber como converter uma string para letras minúsculas. Isso pode ser útil para fins de comparação ou formatação consistente.

## Como Fazer

Para converter uma string para letras minúsculas em Gleam, você pode usar a função `to_lower_case` do módulo `gleam/string`:

```Gleam
import gleam/string

let nome = "GLEAM"
let minusculo = string.to_lower_case(nome)

/* output: "gleam" */
```

Você também pode usar a função `to_lower_case` diretamente em uma string:

```Gleam
let nome_minusculo = "gleam" |> string.to_lower_case

/* output: "gleam" */
```

## Mergulho Profundo

A função `to_lower_case` usa a biblioteca Unicode para converter todos os caracteres em letras minúsculas, incluindo caracteres especiais de vários idiomas. Além disso, essa função é imutável, o que significa que uma cópia da string original é retornada com todas as letras em minúsculo.

## Veja Também

- [Gleam Documentation](https://gleam.run/documentation)
- [Gleam Strings Module](https://gleam.run/documentation/stdlib/string.md)
- [Unicode Character Database](https://unicode.org/ucd/)
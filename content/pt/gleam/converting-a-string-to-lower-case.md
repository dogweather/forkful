---
title:                "Gleam: Convertendo uma string para letras minúsculas"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que?

Seja por necessidade de comparação com outras strings ou para simplificar o processamento de dados, converter uma string para caracteres minúsculos é uma tarefa comum em qualquer linguagem de programação, incluindo Gleam.

## Como fazer

Existem várias maneiras de converter uma string para minúsculas em Gleam. Vamos dar uma olhada em duas abordagens diferentes utilizando a função `to_lower_case()`.

### Usando uma biblioteca externa

Para aqueles que preferem utilizar bibliotecas externas, a biblioteca `gleam_strings` possui uma função `to_lower_case()` que pode ser usada da seguinte forma:

```Gleam
// Importar a biblioteca gleam_strings
import gleam_strings

// Definir uma string
let string = "Gleam Programming Rocks!"

// Converter para minúsculas
let lower_case_string = gleam_strings.to_lower_case(string)

// Imprimir o resultado
io.println(lower_case_string)

// Saída: "gleam programming rocks!"
```

### Utilizando a função nativa da linguagem

Gleam também possui uma função nativa para conversão de strings para minúsculas, que pode ser utilizada da seguinte forma:

```Gleam
// Definir uma string
let string = "Gleam Programming Rocks!"

// Converter para minúsculas
let lower_case_string = string.to_lower_case()

// Imprimir o resultado
io.println(lower_case_string)

// Saída: "gleam programming rocks!"
```

## Aprofundando

Ao converter uma string para minúsculas, é importante ter em mente que esse processo não é simplesmente substituir todos os caracteres maiúsculos pelos seus equivalentes minúsculos. Existem casos em que a conversão pode ser mais complexa, como, por exemplo, quando a string contém caracteres com acentos.

Além disso, é sempre recomendável analisar a documentação da função que está sendo utilizada, como parâmetros opcionais que possam afetar o resultado final.

## Veja também

- Documentação da função `to_lower_case()`: https://gleam.run/documentation/stdlib/string#to_lower_case
- Documentação da biblioteca `gleam_strings`: https://hexdocs.pm/gleam_strings/api-reference.html
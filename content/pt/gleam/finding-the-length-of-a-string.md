---
title:                "Gleam: Encontrando o comprimento de uma string"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Por que encontrar o comprimento de uma string é importante?

Encontrar o comprimento de uma string é uma tarefa comum em programação, especialmente ao lidar com dados de entrada do usuário ou ao manipular strings em geral. Saber o comprimento de uma string pode ajudar os programadores a realizar várias operações, como validação de dados, formatação de strings e muito mais. Por isso, é um conceito fundamental que todo programador deve dominar.

# Como encontrar o comprimento de uma string em Gleam

Em Gleam, podemos encontrar o comprimento de uma string usando a função `String.length()`. Veja um exemplo abaixo:

```Gleam
let string = "Olá, mundo!"
let comprimento = String.length(string)
```

Neste exemplo, definimos uma string chamada "Olá, mundo!" e usamos a função `String.length()` para encontrar o seu comprimento. O valor retornado será 12, já que essa é a quantidade de caracteres na string.

Também é importante notar que espaços em branco e caracteres especiais também são contados como caracteres na string. Por exemplo, a string "Olá, mundo!" tem 12 caracteres, mas se incluirmos um espaço em branco no final dela, o comprimento será de 13.

# Mergulho mais profundo

Existem outras maneiras de encontrar o comprimento de uma string em Gleam, como utilizar a sintaxe `#[derive len]` para estruturas personalizadas que implementam o Trait `Length`. Além disso, a função `String.length()` também pode ser aplicada a uma lista de strings para encontrar o comprimento total da lista.

Outro aspecto importante a ser considerado é a diferença entre caracteres e bytes em relação ao comprimento de uma string. Em alguns casos, o comprimento de uma string pode ser diferente do número de bytes que ela ocupa na memória, especialmente quando se trata de usar caracteres multibyte. Isso pode afetar alguns algoritmos e funções que dependem do comprimento de uma string, portanto, é importante estar ciente dessas diferenças.

# Veja também

- Documentação oficial da função `String.length()`: https://gleam.run/core/gleam/String.html#length
- Tutorial de Gleam sobre strings: https://gleam.run/tutorials/strings
- Sintaxe `#[derive len]`: https://gleam.run/tutorials/derive-attribute#length
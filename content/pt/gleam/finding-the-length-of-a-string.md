---
title:                "Gleam: Encontrar o comprimento de uma string"
simple_title:         "Encontrar o comprimento de uma string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que encontrar o comprimento de uma string?

Encontrar o comprimento de uma string é uma tarefa comum em programação, especialmente ao lidar com entrada de dados do usuário ou ao manipular strings em geral. Saber o comprimento de uma string pode ser útil para diferentes propósitos, como validar o tamanho do input do usuário ou fazer operações de manipulação de dados.

## Como fazer:

Para encontrar o comprimento de uma string em Gleam, podemos usar a função `String.length()`. Veja um exemplo abaixo:

```Gleam
let texto = "Olá, mundo!"
let comprimento = String.length(texto)

// Saída:
// comprimento: 12
```

Aqui, declaramos uma variável `texto` que contém a string "Olá, mundo!" e em seguida usamos a função `String.length()` para encontrar o comprimento dessa string. O comprimento será armazenado na variável `comprimento` e o resultado será impresso no console.

## Profundidade:

A função `String.length()` é bastante útil e simples, no entanto, é importante entender como ela funciona por debaixo dos panos. Em essência, ela conta quantos caracteres existem na string, incluindo espaços em branco e caracteres especiais. Portanto, o comprimento de uma string vazia será 0.

Além disso, é importante lembrar que, em alguns idiomas, há caracteres que possuem mais de um byte. Isso significa que o comprimento pode variar dependendo do idioma que a string está sendo escrita. Em Gleam, esses caracteres são contados corretamente e a função `String.length()` retorna o comprimento correto.

## Veja também:

- Documentação oficial da função `String.length()`: [https://gleam.run/docs/stdlib/String.length.html](https://gleam.run/docs/stdlib/String.length.html)
- Tutorial de Gleam: [https://gleam.run/getting-started/](https://gleam.run/getting-started/)
- Lista completa de funções de string em Gleam: [https://gleam.run/docs/stdlib/String.html](https://gleam.run/docs/stdlib/String.html)
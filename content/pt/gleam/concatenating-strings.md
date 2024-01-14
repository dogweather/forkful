---
title:    "Gleam: Concatenando strings"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## PorqueConcatenar Strings em Gleam Programação:

Concatenar strings é uma habilidade essencial em linguagens de programação como Gleam. Permite que você combine diferentes strings de texto para formar uma única string, tornando seu código mais versátil e dinâmico.

## Como Fazer:

Em Gleam, a concatenação de strings é feita usando o operador `++`. Aqui está um exemplo de código em Gleam para ilustrar isso:

```Gleam
let primeiro_nome = "Maria"
let sobrenome = "Silva"

let nome_completo = primeiro_nome ++ " " ++ sobrenome

io.println(nome_completo)
```

A saída desse código será "Maria Silva", demonstrando como as duas strings foram combinadas em uma usando o operador `++`. Você também pode concatenar mais de duas strings usando este operador repetidamente.

Outra opção para concatenar strings é usando a função `string.concat`. Esta função aceita uma lista de strings como argumento e retorna uma string única com todas as strings combinadas. Aqui está um exemplo:

```Gleam
let nomes = ["João", "Ana", "Pedro"]

let lista_nomes = string.concat(nomes)

io.println(lista_nomes)
```

A saída deste código será "JoãoAnaPedro", já que as três strings foram concatenadas em uma única string.

## Mergulho Profundo:

Existem algumas coisas a serem consideradas ao concatenar strings em Gleam. Primeiro, lembre-se de que o operador `++` só pode ser usado para concatenar duas strings. Se você precisar concatenar mais de duas strings, é melhor usar a função `string.concat`.

Além disso, concatenar strings em Gleam pode ser mais eficiente do que em outras linguagens. Isso ocorre porque Gleam usa imutabilidade, o que significa que quando você concatena duas strings, na verdade está criando uma nova string em vez de alterar as strings originais. Isso pode ser útil quando você precisa concatenar muitas strings em um loop, pois não há sobrecarga de memória.

Finalmente, é importante lembrar que as strings em Gleam são representadas usando o tipo `binary`, o que significa que elas são armazenadas como um array de bytes. Isso pode ser útil ao lidar com caracteres especiais, mas também significa que os índices dos caracteres podem ser diferentes do que você esperava. Portanto, é importante ter cuidado ao usar funções de índice em strings concatenadas.

## Veja Também:

- [Documentação Oficial do Gleam sobre Concatenação de Strings](https://gleam.run/documentation/guide/strings/#concatenation)
- [Tutorial de Gleam no FreeCodeCamp](https://www.freecodecamp.org/news/a-quick-introduction-to-gleam-programming/)
- [Fórum Gleam para discussões e suporte](https://gleam.discourse.group)
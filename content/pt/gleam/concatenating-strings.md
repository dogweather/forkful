---
title:                "Gleam: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

# Por que concatenar strings?

Ao escrever código em qualquer linguagem de programação, muitas vezes precisamos unir strings, ou seja, juntar duas ou mais cadeias de caracteres para formar uma única string. Isso é especialmente útil quando estamos trabalhando com dados de entrada do usuário ou quando precisamos construir uma mensagem dinâmica.

# Como fazer

Em Gleam, podemos concatenar strings usando o operador `++` ou a função embutida `string.concat()`. Vamos ver alguns exemplos:

### Usando o operador `++`

```Gleam
let greeting = "Olá, "
let name = "Maria"
let message = greeting ++ name
```

Neste exemplo, `message` terá o valor "Olá, Maria". Podemos também concatenar mais de duas strings:

```Gleam
let first_name = "João"
let last_name = "Silva"
let full_name = first_name ++ " " ++ last_name
```

Agora `full_name` será igual a "João Silva".

### Usando a função `string.concat()`

```Gleam
let greeting = "Bom dia, "
let pronoun = "você"
let message = string.concat([greeting, pronoun])
```

Aqui, `message` será "Bom dia, você". A função `string.concat()` também nos permite concatenar mais de duas strings ao passar uma lista de strings como argumento.

# Aprofundando

Ao concatenar strings, é importante lembrar que o tipo resultante sempre será uma string. Isso significa que podemos combinar strings com outros tipos de dados, como inteiros ou booleanos, sem se preocupar com possíveis erros de tipo.

Também é importante mencionar que usar o operador `++` muitas vezes pode levar a uma má performance devido à criação frequentemente desnecessária de novas strings. Por isso, é recomendado utilizar a função `string.concat()` sempre que possível para otimizar seu código.

# Veja também

- Documentação oficial sobre strings em Gleam: https://gleam.run/documentation/stdlib#string
- Tutorial sobre concatenação de strings em Gleam: https://gleam.run/getting-started/strings#concatenating-strings
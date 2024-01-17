---
title:                "Encontrando o comprimento de uma string"
html_title:           "Javascript: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# O que & Por quê?

A função de encontrar o comprimento de uma string é uma tarefa comum para programadores de Javascript. O comprimento de uma string se refere ao número de caracteres contidos nela, incluindo espaços e pontuações. Programadores usam essa funcionalidade para realizar diversas ações, como validar dados ou gerar resultados de forma dinâmica.

# Como fazer:

```Javascript
// Utilizando a propriedade length
let string = "Olá amigos!"
console.log(string.length)
// Output: 12

// Utilizando o método .length()
let nome = "Maria"
console.log(nome.length)
// Output: 5
```

# Profundidade Técnica:

A funcionalidade de encontrar o comprimento de uma string foi introduzida como uma propriedade do objeto String em ECMAScript 1 (também conhecido como Javascript 1.0). Esta propriedade é acessível através do operador `.` e é uma propriedade somente leitura, o que significa que não pode ser modificada. Alternativamente, é possível utilizar o método `.length()` que realiza a mesma tarefa. É importante ter em mente que, em ambos os casos, os espaços e pontuações também são incluídos na contagem.

# Veja também:

- Documentação do MDN: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/length
- Vídeo tutorial (em inglês): https://www.youtube.com/watch?v=ZJ8vXiAFC8c
- Prática da função: https://www.youtube.com/watch?v=BTjE72fQxbA
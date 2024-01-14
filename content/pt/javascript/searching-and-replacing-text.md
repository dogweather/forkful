---
title:                "Javascript: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

Por que o Search and Replace é uma habilidade essencial em programação

Se você é um programador, provavelmente já ouviu falar sobre a habilidade de busca e substituição (search and replace) em sua linguagem de programação preferida. Mas por que é tão importante dominar essa habilidade? A resposta é simples: ela economiza tempo e evita erros repetitivos.

Como fazer busca e substituição em Javascript

Para realizar busca e substituição em Javascript, você pode usar o método `replace()` em uma string. Vamos ver um exemplo de como usar esse método para substituir todas as ocorrências de uma palavra em uma frase:

```Javascript
let frase = "Eu gosto de comer maçãs vermelhas."
let novaFrase = frase.replace("maçãs vermelhas", "uvas roxas");
console.log(novaFrase);
```

O resultado será "Eu gosto de comer uvas roxas." Nesse exemplo, usamos o método `replace()` para substituir "maçãs vermelhas" por "uvas roxas".

Você também pode usar expressões regulares para realizar busca e substituição em um texto. Por exemplo, se quisermos substituir todas as ocorrências de números por asteriscos em uma string, podemos fazer da seguinte forma:

```Javascript
let texto = "Existem 10 tipos de pessoas no mundo, as que entendem binário e as que não entendem."
let novoTexto = texto.replace(/\d+/g, "*");
console.log(novoTexto);
```

O resultado será "Existem * tipos de pessoas no mundo, as que entendem binário e as que não entendem." Nesse exemplo, usamos a expressão regular `/\d+/g` para selecionar todos os números e substituí-los por asteriscos.

Aprofundando na busca e substituição em Javascript

Para realizar uma busca e substituição mais complexa em Javascript, é importante entender as diferentes opções que o método `replace()` oferece. Por exemplo, você pode usar a flag `i` para tornar a busca case-insensitive (não diferenciar maiúsculas e minúsculas) ou a flag `g` para substituir todas as ocorrências em vez de apenas a primeira.

Também é possível usar o método `replace()` em uma expressão regular, em vez de em uma string. Isso pode ser útil quando você precisa substituir apenas partes específicas de uma string, e não todas as ocorrências.

Veja mais exemplos e aprofunde-se nessa habilidade indispensável para programadores em [documentação do método `replace()`] (https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/replace) e [tutoriais sobre expressões regulares em Javascript] (https://blog.betrybe.com/desenvolvimento-web/expressoes-regulares-javascript/).

Veja também

Aprenda outras habilidades importantes em Javascript e acompanhe as novidades na linguagem com os seguintes links:

- [Laço for em Javascript] (https://junior314.medium.com/o-la%C3%A7o-for-em-javascript-2944d6d1a596)
- [Novos recursos do ECMAScript 6] (https://www.devmedia.com.br/es6-os-6-novos-recursos-do-javascript/33730)
- [Design patterns em Javascript] (https://imasters.com.br/desenvolvimento/patterns-em-javascript-singleton-factory-builder-observer-command)
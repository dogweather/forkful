---
title:                "Utilizando expressões regulares"
html_title:           "Javascript: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares em Javascript?

Expressões regulares são um recurso poderoso que permite buscar, validar e manipular texto de forma eficiente em Javascript. Com elas, é possível encontrar padrões específicos de caracteres em strings, tornando o processo de manipulação de dados muito mais rápido e preciso.

## Como usar expressões regulares em Javascript?

Para utilizar expressões regulares em Javascript, primeiro é necessário conhecer a sintaxe básica. Elas são escritas entre barras (/) e podem conter diferentes caracteres especiais que representam diferentes tipos de busca. Por exemplo, a expressão regular /test/ irá buscar por todas as ocorrências da palavra "test" em uma string.

```Javascript
// Exemplo de busca por "test"
let string = "Esse é um teste de expressão regular."
let regex = /test/;

console.log(string.match(regex)); // Resultado: [ "test" ]
```

Podemos utilizar a flag "i" após a barra para tornar a busca case-insensitive, ou seja, sem diferenciar entre letras maiúsculas e minúsculas.

```Javascript
// Exemplo de busca case-insensitive por "test"
let string = "Esse é um TESTE de expressão regular."
let regex = /test/i;

console.log(string.match(regex)); // Resultado: [ "TESTE" ]
```

Além disso, expressões regulares também contam com metacaracteres que representam diferentes tipos de caracteres. Alguns exemplos são:

- \w: busca por qualquer caractere alfanumérico.
- \d: busca por qualquer dígito numérico.
- \s: busca por qualquer espaço em branco.

```Javascript
// Exemplo de busca por dígitos numéricos
let string = "2345 bananas"
let regex = /\d+/;

console.log(string.match(regex)); // Resultado: [ "2345" ]
```

É importante ressaltar que, além de buscar por padrões, as expressões regulares também podem ser utilizadas para substituir ou validar determinados caracteres em uma string.

## Mais informações sobre expressões regulares

Existem muitos recursos e funcionalidades disponíveis para o uso de expressões regulares em Javascript. A documentação oficial da linguagem é um ótimo lugar para iniciar os estudos e aprender mais sobre suas aplicações. Além disso, também existem muitos tutoriais e exemplos na internet que podem auxiliar na compreensão e utilização dessa poderosa ferramenta.

## Veja também

- Expressões regulares em Javascript: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Regular_Expressions
- Tutorial completo sobre expressões regulares: https://www.regular-expressions.info/tutorial.html
- Documentação oficial do Javascript: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript
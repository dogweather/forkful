---
title:                "Javascript: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Por que converter uma string para minúsculas?

Quando se trabalha com programação, é comum manipularmos strings (cadeias de caracteres). Em alguns casos, é necessário que essas strings estejam em minúsculo, seja para facilitar a comparação de textos ou para uma melhor formatação de saída. Por isso, a conversão de uma string para minúscula é um recurso importante para os desenvolvedores.

## Como fazer a conversão em Javascript

Existem diversas formas de se converter uma string para minúsculas em Javascript. Vamos mostrar algumas das mais comuns utilizando os métodos `toLowerCase()` e `toLocaleLowerCase()`.

### Utilizando o método `toLowerCase()`

O método `toLowerCase()` converte todos os caracteres de uma string em letras minúsculas. Veja o exemplo abaixo:

```Javascript
const texto = "CONVERSAO DE TEXTO"
const textoEmMinusculo = texto.toLowerCase()
console.log(textoEmMinusculo) // saída: conversao de texto
```

### Utilizando o método `toLocaleLowerCase()`

O método `toLocaleLowerCase()` também converte os caracteres de uma string em minúsculas, mas leva em consideração a localidade do sistema. Isso significa que a conversão pode variar de acordo com o idioma utilizado no navegador. Veja o exemplo abaixo:

```Javascript
const texto = "ção é uma vogal"
const textoEmMinusculo = texto.toLocaleLowerCase()
console.log(textoEmMinusculo) // saída: ção é uma vogal (no navegador em português)
console.log(textoEmMinusculo) // saída: c~ao é uma vogal (no navegador em espanhol)
```

## Aprofundando no assunto

O Javascript possui um objeto chamado `String`, que contém diversos métodos para manipulação de strings. O método `toLowerCase()` é apenas um deles. Além disso, é possível realizar a conversão de minúsculas para maiúsculas utilizando os métodos `toUpperCase()` e `toLocaleUpperCase()`, respectivamente.

É importante lembrar que a conversão de uma string para minúsculas não altera o valor da variável original, apenas retorna uma nova string convertida. Ou seja, caso seja necessário utilizar a string em minúsculo, é importante armazenar o resultado em uma nova variável.

# Veja também

- [Documentação oficial do método `toLowerCase()` (em inglês)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Documentação oficial do método `toLocaleLowerCase()` (em inglês)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)
- [Outros métodos do objeto `String` (em inglês)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
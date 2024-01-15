---
title:                "Transformando uma string em maiúsculas"
html_title:           "Javascript: Transformando uma string em maiúsculas"
simple_title:         "Transformando uma string em maiúsculas"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que

Há muitas razões pelas quais alguém pode querer capitalizar uma string em Javascript. Um dos motivos mais comuns é para tornar o texto mais legível, especialmente em casos em que a string é gerada dinamicamente e pode conter caracteres minúsculos e maiúsculos misturados.

## Como Fazer

Existem várias maneiras de capitalizar uma string em Javascript, mas uma das abordagens mais simples é usar o método `toUpperCase()`. Este método transforma todos os caracteres em uma string em maiúsculas e retorna a nova string capitalizada.

```Javascript
let str = "exemplo de string";
let strCapitalizada = str.toUpperCase();

console.log(strCapitalizada); // Saída: "EXEMPLO DE STRING"
```

Se você precisa capitalizar apenas a primeira letra de uma string, pode usar a seguinte função:

```Javascript
function capitalizarPrimeiraLetra(str) {
    return str.charAt(0).toUpperCase() + str.slice(1);
}

let str = "exemplo de string";
let strCapitalizada = capitalizarPrimeiraLetra(str);

console.log(strCapitalizada); // Saída: "Exemplo de string"
```

Outra forma de capitalizar uma string é usando expressões regulares. Neste exemplo, usaremos a função `replace()` para encontrar a primeira letra de cada palavra (ou seja, após cada espaço) e transformá-la em maiúscula.

```Javascript
function capitalizarPalavras(str) {
    return str.replace(/\b[a-z]/g, function(letra) {
        return letra.toUpperCase();
    });
}

let str = "exemplo de string";
let strCapitalizada = capitalizarPalavras(str);

console.log(strCapitalizada); // Saída: "Exemplo De String"
```

## Deep Dive

Ao capitalizar uma string em Javascript, é importante lembrar que a função `toUpperCase()` e a técnica usando expressões regulares levam em consideração o idioma padrão do navegador do usuário. Isso significa que, se o usuário estiver usando um idioma diferente, o resultado da capitalização pode ser diferente do esperado.

Outra consideração importante é que o método `toUpperCase()` não altera a string original, mas retorna uma nova string capitalizada. Portanto, se você quiser modificar a string original, deve atribuir o resultado a ela ou usar um método como `replace()` para atualizá-la.

## Veja Também

- [Documentação do método `toUpperCase()`](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Expressões regulares em JavaScript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Regular_Expressions)
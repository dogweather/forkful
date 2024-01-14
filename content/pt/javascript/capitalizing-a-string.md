---
title:    "Javascript: Capitalizando uma string"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string?

Capitalizar uma string é um processo importante na programação, especialmente quando se trabalha com dados do usuário. Ele permite que o texto seja padronizado e mais legível, tornando-o útil para diversas aplicações, como validação de formulários ou exibição de informações. Neste artigo, vamos explorar como capitalizar uma string em Javascript.

## Como fazer:

Para capitalizar uma string em Javascript, podemos utilizar funções nativas e métodos de arrays, como o `charAt` e `toUpperCase`. Veja um exemplo abaixo:

```Javascript
// Definindo a string de entrada
let string = "chocolate";

// Criando uma função para capitalizar a primeira letra
function capitalize(str) {
    // Separando a primeira letra e o restante da string
    let firstLetter = str.charAt(0);
    let restOfStr = str.slice(1);

    // Transformando a primeira letra em maiúscula 
    let capitalizedStr = firstLetter.toUpperCase() + restOfStr;

    return capitalizedStr;
}

// Imprime o resultado: "Chocolate"
console.log(capitalize(string));
```

Neste exemplo, a função `capitalize` recebe uma string como argumento e retorna a string com a primeira letra em maiúscula. É importante notar que a função `toUpperCase` não altera a string original, mas retorna uma nova string com a letra maiúscula.

## Mais detalhes sobre capitalização de strings:

Existem outras maneiras de capitalizar uma string em Javascript, como utilizar expressões regulares ou bibliotecas externas. Também é possível capitalizar apenas a primeira letra de cada palavra em uma frase, ou até mesmo criar funções personalizadas que atendam às necessidades específicas do seu projeto.

Outro aspecto importante ao capitalizar strings é a questão da localização. Dependendo do idioma e alfabeto utilizado, as regras para capitalização podem variar. É sempre bom estar ciente dessas diferenças ao lidar com dados do usuário.

## Veja também:

- [Documentação oficial do método `toUpperCase()` em Javascript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Guia de expressões regulares em Javascript](https://braziljs.org/blog/guia-de-expressoes-regulares/)
- [Biblioteca Lodash para manipulação de strings](https://lodash.com/docs/4.17.15#capitalize)
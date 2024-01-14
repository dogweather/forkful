---
title:                "Javascript: Convertendo uma string para minúsculas"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que converter uma string para letras minúsculas

Ao trabalhar com textos em Javascript, é comum que seja necessário converter uma string para letras minúsculas. Isso pode ser útil para realizar comparações, tornar o texto mais legível ou simplesmente seguir convenções de nomenclatura. Neste artigo, vamos explorar como realizar essa conversão e entender melhor sua importância.

## Como fazer a conversão

Para converter uma string para letras minúsculas em Javascript, podemos utilizar o método `toLowerCase()` que está presente no objeto String. Este método retorna uma nova string com todas as letras em minúsculo, mantendo o texto original inalterado. Veja abaixo um exemplo de como utilizá-lo:

```Javascript
let meuTexto = "Olá, Mundo!";
let textoMinusculo = meuTexto.toLowerCase();
console.log(textoMinusculo); // saída: "olá, mundo!"
```

Como podemos ver no código acima, o método `toLowerCase()` pode ser chamado diretamente através da variável que armazena nossa string. Isso torna a conversão muito simples e direta.

Outra forma de realizar a conversão é através do uso de expressões regulares. Por exemplo, podemos utilizar a função `replace()` para substituir todas as letras maiúsculas por suas versões minúsculas. Veja:

```Javascript
let meuTexto = "Olá, Mundo!";
let textoMinusculo = meuTexto.replace(/[A-Z]/g, (letraMaiuscula) => letraMaiuscula.toLowerCase());
console.log(textoMinusculo); // saída: "olá, mundo!"
```

Neste caso, utilizamos uma expressão regular para encontrar todas as letras maiúsculas na nossa string e substituí-las pelo seu equivalente em minúsculo. A função `replace()` recebe como parâmetro a expressão regular e uma função que irá retornar o valor de substituição.

## Aprofundando na conversão

É importante lembrar que a conversão para letras minúsculas no Javascript leva em consideração o padrão Unicode. Isso significa que alguns caracteres especiais de outros idiomas podem ter seu comportamento alterado durante a conversão. Por exemplo, em certos idiomas a letra "I" não possui um equivalente em minúsculo, portanto, ela não será convertida corretamente. É importante ter isso em mente ao lidar com diferentes idiomas em suas aplicações.

Além disso, vale ressaltar que o método `toLowerCase()` não altera a string original, mas sim cria uma nova string com o resultado da conversão. Isso garante que o texto original não seja modificado e possa ser utilizado em outras partes do código como necessário.

## Veja também

- Documentação oficial do método `toLowerCase()` (em inglês): https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase
- Expressões regulares em Javascript (em português): https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Regular_Expressions
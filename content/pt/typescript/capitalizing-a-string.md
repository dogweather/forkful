---
title:                "TypeScript: Maiúsculas em uma sequência de caracteres"
simple_title:         "Maiúsculas em uma sequência de caracteres"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string?

Capitalizar uma string é um processo comum na programação que envolve alterar a primeira letra de cada palavra em uma string para maiúscula. Isso pode ser útil para padronizar a formatação de uma string ou para melhorar a legibilidade do código.

## Como fazer?

```TypeScript
const capitalizarString = (str: string) => {
  const palavras = str.split(" ");
  const novaString = [];

  for (let i = 0; i < palavras.length; i++) {
    const primeiraLetra = palavras[i].charAt(0).toUpperCase();
    const restoDaPalavra = palavras[i].slice(1);
    const palavraCapitalizada = primeiraLetra + restoDaPalavra;
    novaString.push(palavraCapitalizada);
  }

  return novaString.join(" ");
}

capitalizarString("isso é um exemplo"); // Output: "Isso É Um Exemplo"
```

Neste exemplo, criamos uma função que recebe uma string como parâmetro e a divide em uma array de palavras. Em seguida, percorremos essa array e alteramos a primeira letra de cada palavra para maiúscula, concatenando com o restante da palavra. Por fim, juntamos novamente todas as palavras em uma única string utilizando o método `join()`.

## Profundidade do assunto

Existem outras maneiras de capitalizar uma string em TypeScript, como utilizando as funções `charAt()` e `toUpperCase()` diretamente na string, ou até mesmo expressões regulares. Além disso, também é possível utilizar bibliotecas externas que oferecem recursos mais avançados para manipulação de strings.

## Veja também

- [Guia completo sobre strings em TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html)
- [Documentação do método `toUpperCase()`](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Documentação do método `charAt()`](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
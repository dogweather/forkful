---
title:                "TypeScript: Extraindo subcadeias"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings em TypeScript?

A extração de substrings é uma técnica comum e útil em muitas linguagens de programação, incluindo TypeScript. Ela permite que você selecione uma parte específica de uma string, o que pode ser muito útil em várias situações. Nesta postagem, vamos explorar como e por que usar a extração de substrings em seus projetos TypeScript.

## Como extrair substrings em TypeScript

Em TypeScript, a extração de substrings pode ser realizada usando o método `substring()` de uma string. Este método aceita dois parâmetros: o índice inicial e o índice final da substring que você deseja extrair. Vamos dar uma olhada em um exemplo:

```TypeScript
let str = "Ola Mundo";
let substring = str.substring(0, 3); // seleciona os caracteres nas posições 0 a 2

console.log(substring); // saída: Ola
```

No código acima, nós criamos uma variável `str` que contém a string "Ola Mundo". Em seguida, usamos o método `substring()` para extrair os caracteres nas posições 0 a 2, que correspondem a "O", "l" e "a". A substring resultante é atribuída à nossa variável `substring` e, em seguida, é impressa no console.

Você também pode usar o método `substring()` com um único parâmetro, que indica o índice inicial da substring. Nesse caso, a substring será extraída até o final da string. Veja um exemplo:

```TypeScript
let str = "Hello World";
let substring = str.substring(6); // seleciona os caracteres da posição 6 até o final

console.log(substring); // saída: World
```

Este é apenas um exemplo básico de como extrair substrings em TypeScript. Existem muitas outras possibilidades e variações em como usar esse método, então certifique-se de ler a documentação oficial para mais informações.

## Mais informações sobre a extração de substrings

Se você quiser se aprofundar ainda mais na extração de substrings em TypeScript, aqui estão algumas coisas adicionais que você pode aprender:

- Além do método `substring()`, também é possível usar o método `slice()` para extrair substrings em TypeScript. A diferença é que o `slice()` permite usar índices negativos, o que torna mais fácil selecionar caracteres a partir do final da string.
- Você pode usar a propriedade `length` de uma string para obter o total de caracteres e, assim, selecionar uma substring que abranja toda a string original.
- Existem outras funções e métodos relacionados à manipulação de strings em TypeScript, como `charAt()`, `indexOf()` e `lastIndexOf()`, que podem ser úteis ao trabalhar com substrings.

## Veja também

Aqui estão alguns recursos úteis para saber mais sobre a extração de substrings em TypeScript:

- [Documentação oficial do método `substring()`](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [Documentação oficial do método `slice()`](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [Tutorial em vídeo sobre extração de substrings em TypeScript](https://www.youtube.com/watch?v=UJlR3QADMdo)

Esperamos que este artigo tenha sido útil para você entender melhor a extração de substrings em TypeScript. Agora você pode aplicar esse conhecimento em seus próprios projetos e torná-los ainda mais poderosos!
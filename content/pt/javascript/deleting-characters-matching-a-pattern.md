---
title:    "Javascript: Excluindo caracteres que correspondem a um padrão"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Por que deletar caracteres que correspondem a um padrão?

Às vezes, em programação, precisamos lidar com dados que possuem caracteres extras ou desnecessários que podem atrapalhar nosso código. Nesses casos, é útil e eficiente remover esses caracteres e deixar apenas o que realmente importa. A exclusão de caracteres que correspondem a um padrão é uma maneira de limpar e organizar nossos dados de forma mais eficaz.

## Como fazer

Para deletar caracteres que correspondem a um padrão em Javascript, podemos usar a função "replace", que substitui caracteres especificados por um novo valor. Primeiro, definimos uma variável com nosso texto original e, em seguida, usamos o método "replace" especificando o padrão que queremos excluir e o valor vazio, que representa nada, para que os caracteres sejam removidos. Aqui está um exemplo de código:

```Javascript
let texto = "Este é um exemplo de texto com caracteres desnecessários!";
texto = texto.replace(/[^a-zA-Z ]/g, "");
```

Neste exemplo, usamos a expressão regular /[^a-zA-Z ]/g para especificar que queremos excluir todos os caracteres que não são letras ou espaços em branco. O modificador "g" garante que todos os caracteres correspondentes sejam substituídos. O resultado do código acima seria: "Este é um exemplo de texto com caracteres desnecessários". Agora nosso texto está limpo e sem os caracteres indesejados.

## Mergulho Profundo

Existem muitas outras maneiras de excluir caracteres que correspondem a um padrão em Javascript. Podemos usar outras expressões regulares, como "\W", que exclui todos os caracteres não alfanuméricos, ou podemos usar o método "split", que divide uma string em um array usando um separador especificado e, em seguida, juntar os elementos do array novamente em uma string sem os caracteres indesejados.

Também é importante lembrar que existem casos em que queremos manter alguns caracteres, mesmo se eles corresponderem ao padrão. Nesses casos, podemos usar grupos de captura em nossas expressões regulares e referenciá-los no valor de substituição.

A exclusão de caracteres que correspondem a um padrão pode ser muito útil em situações em que precisamos limpar nossos dados ou manipular strings de uma maneira específica. Conhecer as diferentes maneiras de fazer isso em Javascript pode melhorar nossa eficiência e tornar nosso código mais organizado.

## Veja também

- [Guia de expressões regulares em Javascript](https://www.regular-expressions.info/javascript.html)
- [Documentação do método replace em Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Blog post sobre o uso de grupos de captura em expressões regulares em Javascript](https://javascript.plainenglish.io/a-simple-introduction-to-javascript-regular-expressions-for-beginners-ad61338e74dc?sk=7e35aa74925681e1796c022ce2b65ba3)
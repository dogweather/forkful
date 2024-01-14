---
title:    "TypeScript: Excluindo caracteres que correspondem a um padrão"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Às vezes, durante o processo de programação, é necessário limpar e organizar uma string removendo certos caracteres que correspondem a um determinado padrão. Isso pode ser útil quando se trabalha com dados de entrada desorganizados ou quando se deseja formatar uma string de uma maneira específica.

## Como fazer

Para deletar caracteres que correspondem a um padrão em TypeScript, podemos usar o método "replace" da classe String. Isso nos permite substituir os caracteres indesejados por uma string vazia, removendo-os da string final.

```TypeScript
let texto: string = "Olá, Meu! Nome é *João*.";
let novoTexto: string = texto.replace(/[*!]/g, "");
// novoTexto é agora "Olá, Meu Nome é João."
```

No exemplo acima, usamos uma expressão regular para correspondência de padrões, representada pelos caracteres entre as barras. O "g" no final da expressão regular significa que ela deve ser aplicada globalmente em toda a string. É importante lembrar que expressões regulares são case sensitive, então é necessário garantir que os caracteres a serem removidos estejam na mesma caixa que na string original.

## Aprofundamento

O uso de expressões regulares para correspondência de padrões é uma técnica poderosa em programação. Além de deletar caracteres em uma string, também podemos usá-las para buscar, substituir e formatar dados de maneira eficiente. É possível explorar mais sobre expressões regulares e suas aplicações em TypeScript em recursos como o [tutorial do TypeScript](https://www.typescriptlang.org/docs/handbook/regular-expressions.html) ou [este artigo da MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions).

## Veja também

- [Documentação do método "replace" em String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Artigo sobre expressões regulares em TypeScript](https://medium.com/@vladtataranu/how-to-use-regular-expressions-in-typescript-b4346cc61f4a)
- [Ferramenta online para testar e criar expressões regulares](https://regexr.com/)
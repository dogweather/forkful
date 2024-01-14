---
title:                "TypeScript: Convertendo uma sequência de caracteres para minúsculas"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que é importante converter uma string para letras minúsculas?

A conversão de uma string para letras minúsculas é importante para garantir a consistência e eficiência do seu código. Ela permite que você compare e manipule strings de forma mais precisa e confiável.

## Como fazer a conversão em TypeScript

```TypeScript
const stringMaiuscula: string = "EXEMPLO";
const stringMinuscula: string = stringMaiuscula.toLowerCase();

console.log(stringMinuscula); // output: "exemplo"
```

Neste exemplo, usamos o método `toLowerCase()` para converter a string `"EXEMPLO"` para `"exemplo"`. Este método retornará uma nova string com todas as letras minúsculas. Lembre-se de atribuir a nova string a uma variável, caso contrário, a string original permanecerá inalterada.

Outro método útil é `toLocaleLowerCase()` que leva em consideração as configurações regionais do sistema operacional ao converter as letras para minúsculas.

```TypeScript
const stringMinuscula: string = "IÑTËRNÂTIÔNĀLIZÆTIØN".toLocaleLowerCase("pt-BR");

console.log(stringMinuscula); // output: "iñtërnâtiônālizætiøn"
```

Além disso, é importante lembrar que em TypeScript, as strings são imutáveis, o que significa que os métodos de conversão não alteram a string original e sempre retornam uma nova string.

## Detalhes sobre a conversão de string para letras minúsculas

A função `toLowerCase()` utiliza a tabela ASCII para converter caracteres para letras minúsculas, portanto, podem haver variações de acordo com a linguagem ou caracteres especiais. É importante fazer um teste minucioso e considerar cenários diferentes ao usar a conversão de strings para letras minúsculas em seu código.

Outra coisa a ter em mente é que a conversão para letras minúsculas não é apenas útil para comparações, mas também pode ser usada para formatar strings, como em títulos e subtítulos.

## Veja também
- Documentação oficial do TypeScript para métodos de conversão de strings: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-1.html#improved-tostring
- Mais dicas sobre uso de strings em TypeScript: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String
- Exemplos de código em TypeScript: https://www.typescriptlang.org/docs/handbook/basic-types.html#string

Esperamos que este artigo tenha sido útil para você entender como converter uma string para letras minúsculas em TypeScript. Lembre-se de sempre considerar os diferentes cenários e testar seu código antes de implementá-lo em produção. Até a próxima!
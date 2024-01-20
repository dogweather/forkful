---
title:                "Capitalizando uma string"
html_title:           "TypeScript: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O que e Por quê?

Capitalizar uma string significa transformar a primeira letra de cada palavra em maiúscula. Programadores fazem isso para melhorar a apresentação dos textos para os usuários, como nomes próprios ou a primeira palavra de cada frase.
    
## Como fazer:

Aqui está uma função simples para capitalizar uma string em TypeScript. 

```TypeScript
function capitalizarTexto(texto: string): string {
    return texto
        .split(' ')
        .map(palavra => palavra.charAt(0).toUpperCase() + palavra.slice(1))
        .join(' ');
}

const nome = 'joão da silva';
console.log(capitalizarTexto(nome));  // Saída: "João Da Silva"
```

Este código divide a string em palavras, capitaliza cada palavra e depois une as palavras novamente em uma única string.

## Análise Detalhada

A capitalização de strings não é um conceito novo em programação. Ela existe desde os primórdios da computação e é uma operação fundamental na manipulação de strings.

Existem outras formas de capitalizar uma string, como usando expressões regulares ou a função `replace`. No entanto, a abordagem apresentada acima é mais simples e mais fácil de entender.

Internamente, a função `toUpperCase` converte uma letra minúscula em maiúscula por meio de um mapeamento definido na tabela de caracteres Unicode. Enquanto a função `charAt` é usada para obter o primeiro carácter de uma palavra, e a função `slice` é usada para obter o resto da palavra.

## Veja Também

Para mais informações sobre a manipulação de strings em TypeScript, você pode consultar os seguintes links:

- Documentação oficial do TypeScript: [String](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- Stack Overflow: [How do you change string to uppercase in JavaScript?](https://stackoverflow.com/questions/1026069/how-do-you-change-string-to-uppercase-in-javascript)
- Mozilla Developer Network: [Glossário: String](https://developer.mozilla.org/pt-BR/docs/Glossary/String)
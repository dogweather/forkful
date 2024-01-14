---
title:    "TypeScript: Extraindo Substrings"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

# Por que extrair substrings em TypeScript?

Você pode estar se perguntando, por que alguém iria querer extrair substrings em TypeScript? Bem, isso pode ser útil para diversas tarefas de programação, como manipulação de strings, validação de dados e formatação de inputs.

## Como fazer em TypeScript

Extrair substrings em TypeScript é bastante simples. Existem duas maneiras principais de fazer isso: utilizando o método `substring()` ou o operador de espalhamento (`...`). Veja alguns exemplos abaixo:

```TypeScript
// Utilizando o método substring()

const frase = "Eu amo TypeScript";

// Extrai a substring "TypeScript"
const substring = frase.substring(6, 16);

console.log(substring); // Saída: TypeScript

// Utilizando o operador de espalhamento (...)

const nome = "Maria";

// Espalha os caracteres do nome em um array
const letras = [...nome];

console.log(letras); // Saída: ["M", "a", "r", "i", "a"]
```

## Mergulho Profundo

Além das formas básicas de extrair substrings, o TypeScript também oferece algumas funções avançadas para trabalhar com strings. Uma delas é o método `slice()`, que permite extrair uma parte de uma string com base em índices. Outra é o operador `substr()`, que permite extrair uma substring com base em uma posição inicial e no tamanho desejado. É importante lembrar que todas essas opções consideram os índices começando em 0.

Além disso, o TypeScript também possui recursos de expressões regulares, que podem ser muito úteis para extrair substrings com base em regras específicas.

# Veja também

- Documentação oficial do TypeScript sobre manipulação de strings: https://www.typescriptlang.org/docs/handbook/strings.html
- Artigo do Medium sobre manipulação de strings em TypeScript: https://medium.com/@krunallathiya/typescript-string-interpolation-and-string-manipulation-with-example-ecc6cef94979
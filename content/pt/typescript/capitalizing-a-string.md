---
title:    "TypeScript: Capitalizando uma string"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

# Por que capitalizar uma string em TypeScript: 

Capitalizar uma string em TypeScript é útil quando se deseja alterar a aparência de uma palavra. Isso pode ser necessário para melhorar a legibilidade ou para cumprir um padrão específico de formatação de dados. 

## Como capitalizar uma string em TypeScript:

```TypeScript
let palavra: string = "javascript";
console.log(palavra.toUpperCase()); // JS
console.log(palavra[0].toUpperCase() + palavra.slice(1)); // Javascript
```

Neste exemplo, definimos uma variável com o valor de "javascript" e utilizamos o método `toUpperCase()` para transformá-la em letras maiúsculas. Também podemos utilizar o método `slice()` para selecionar uma letra específica da palavra e assim, capitalizá-la individualmente.

## Deep Dive: 

A função `toUpperCase()` é um dos métodos padrões disponíveis para strings em TypeScript. Ele transforma todos os caracteres em letras maiúsculas, o que pode ser útil para apresentação de dados em um layout mais organizado. Além disso, podemos implementar variações desse método para atingir objetivos específicos de formatação de strings. 

# Veja também: 

- Documentação Oficial do TypeScript: https://www.typescriptlang.org/docs/
- Tutorial de Strings em TypeScript: https://www.w3schools.com/js/js_strings.asp 
- Exemplos práticos de formatação de strings em TypeScript: https://www.geeksforgeeks.org/string-methods-in-typescript/
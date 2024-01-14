---
title:                "TypeScript: Capitalizando uma string"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string?
 
Capitalizar uma string é uma tarefa comum na programação, especialmente quando lidamos com dados de entrada do usuário. Ao capitalizar uma string, estamos tornando-a mais legível e padronizada, o que pode ser útil para a usabilidade do nosso código. Além disso, é uma boa prática de programação que pode evitar erros em potencial.

## Como fazer

Para capitalizar uma string em TypeScript, podemos seguir alguns passos simples:

1. Definir a string desejada.
2. Usar o método `toUpperCase()` para converter todos os caracteres da string em maiúsculos.
3. Usar o método `charAt()` para obter o primeiro caractere da string e convertê-lo em maiúsculo.
4. Usar o método `slice()` para obter o restante da string a partir do segundo caractere.
5. Juntar o primeiro caractere em maiúsculo com o restante da string em maiúsculo usando o operador `+`.

Aqui está um exemplo de código TypeScript para capitalizar uma string:

```
let string = "programação em typescript";
let capitalizedString = string.charAt(0).toUpperCase() + string.slice(1).toUpperCase();
console.log(capitalizedString); // Saída: Programação em Typescript
```

## Aprofundando mais

Existem outras formas de capitalizar uma string em TypeScript, como por exemplo utilizando as funções `replace()` ou `substring()`. Além disso, é importante lembrar que, se a string contiver acentos ou caracteres especiais, esses métodos podem não funcionar adequadamente.

Uma solução mais completa seria criar uma função que verifique cada caractere da string e, caso necessário, convertê-lo para maiúsculo. Essa função também pode incluir regras adicionais, como manter certas palavras em minúsculo.

## Veja também

- [Documentação oficial do TypeScript](https://www.typescriptlang.org/docs/) 
- [Exemplos de código em TypeScript](https://github.com/Microsoft/TypeScript-Samples)
- [Como usar o TypeScript para criar aplicativos web](https://www.freecodecamp.org/news/getting-started-with-typescript-for-web-app-development-b830afaa1809/)
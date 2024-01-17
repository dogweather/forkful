---
title:                "Imprimindo saída de depuração"
html_title:           "TypeScript: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## O Que e Por Que?

Imprimindo saída de depuração é uma técnica usada por programadores para exibir informações sobre o funcionamento interno de um programa durante a execução. Isso ajuda os programadores a entender como o código está sendo executado e a identificar possíveis erros ou problemas.

## Como Fazer:

Para imprimir saída de depuração em TypeScript, podemos usar a função ```console.log()```. Esta função aceita qualquer número de argumentos e exibe o valor de cada um deles no console do navegador ou do terminal. Veja um exemplo:

```TypeScript 
let idade = 25;
console.log("A minha idade é: ", idade);
```

O console irá exibir a seguinte saída: ```A minha idade é: 25```. Além disso, podemos usar a função ```console.error()``` para imprimir mensagens de erro e ```console.warn()``` para imprimir mensagens de aviso.

## Profundando:

A prática de imprimir saída de depuração remonta aos primórdios da programação, quando apenas podíamos visualizar informações através de impressão em papel. Atualmente, existem outras ferramentas e técnicas disponíveis para depuração de código, como breakpoints em IDEs ou debuggers em navegadores.

No entanto, a impressão de saída de depuração ainda é uma técnica útil e amplamente utilizada, especialmente em casos de depuração de código mais complexo ou em ambientes onde as outras opções não estão disponíveis.

## Veja Também:

- [Documentação oficial do TypeScript](https://www.typescriptlang.org/docs/)
- [Guia de Depuração do Visual Studio Code para TypeScript](https://code.visualstudio.com/docs/nodejs/nodejs-debugging#_typescript)
- [Vídeo tutorial sobre impressão de saída de depuração em TypeScript](https://www.youtube.com/watch?v=Oe421EPjeBE)
---
title:                "TypeScript: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

# Por que imprimir saída de debug?

Não importa o quão experiente você seja em programação, sempre haverá momentos em que você precisará depurar seu código. Imprimir saída de debug pode ser uma ferramenta valiosa para identificar erros e entender o que está acontecendo em seu código. Mesmo que você use um debugger, imprimir a saída em determinados pontos do seu código pode ajudá-lo a visualizar melhor o fluxo de execução e o valor das variáveis em cada etapa.

## Como fazer

Em TypeScript, a maneira mais simples de imprimir saída de debug é usar a função `console.log()`. Basta passar a variável ou valor que você deseja inspecionar como argumento dentro dos parênteses. Por exemplo:

```TypeScript
let nome = "Maria";
let idade = 30;
console.log("Bem-vindo " + nome + ", você tem " + idade + " anos.");
```

Este código imprimirá a seguinte saída: `Bem-vindo Maria, você tem 30 anos.` Você também pode imprimir múltiplas variáveis ou valores separando-os por vírgulas dentro dos parênteses da `console.log()`. Além disso, você pode usar a interpolação de strings para tornar o código mais legível:

```TypeScript
console.log(`O nome é ${nome} e a idade é ${idade}.`);
```

A saída será a mesma, mas desta vez usando interpolação de strings.

## Aprofundando mais

Além de simplesmente imprimir valores, você pode usar a função `console.log()` para mostrar mensagens de erro ou aviso de forma mais descritiva. Em vez de usar o método `console.log()`, você pode optar por `console.error()` ou `console.warn()` para indicar problemas em seu código. Por exemplo:

```TypeScript
if (idade < 18) {
  console.error("Não é permitido acesso para menores de idade.");
} else if (idade < 25) {
  console.warn("Usuário abaixo de 25 anos, certifique-se de seguir as políticas de privacidade.");
}
```

Isso ajudará você a identificar rapidamente onde estão ocorrendo problemas em seu código durante a depuração.

# Veja também

- [Documentação Oficial do TypeScript](https://www.typescriptlang.org/)
- [Guia de Depuração do Visual Studio Code](https://code.visualstudio.com/docs/editor/debugging)
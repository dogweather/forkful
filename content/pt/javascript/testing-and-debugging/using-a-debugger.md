---
date: 2024-01-26 03:49:57.934879-07:00
description: "Usar um depurador significa aproveitar ferramentas especializadas que\
  \ permitem espiar o interior do seu c\xF3digo, acompanhando sua execu\xE7\xE3o passo\
  \ a passo.\u2026"
lastmod: '2024-03-11T00:14:20.702442-06:00'
model: gpt-4-0125-preview
summary: "Usar um depurador significa aproveitar ferramentas especializadas que permitem\
  \ espiar o interior do seu c\xF3digo, acompanhando sua execu\xE7\xE3o passo a passo.\u2026"
title: Usando um depurador
---

{{< edit_this_page >}}

## O Que & Porquê?
Usar um depurador significa aproveitar ferramentas especializadas que permitem espiar o interior do seu código, acompanhando sua execução passo a passo. Os programadores fazem isso para eliminar erros, otimizar o desempenho e entender o comportamento do código.

## Como fazer:
Aqui está um trecho de código JavaScript que não está se comportando conforme o esperado:

```javascript
function buggyMultiply(a, b) {
    return a + b; // Ops! Isso deveria ser uma multiplicação, não adição.
}

let result = buggyMultiply(5, 3);
console.log('Resultado:', result);
```

A saída está incorreta:
```
Resultado: 8
```

Vamos depurar no Chrome DevTools:

1. Abra este JS em um navegador.
2. Clique com o botão direito e selecione "Inspecionar" para abrir o DevTools.
3. Clique na aba "Sources" (Fontes).
4. Encontre seu trecho de código ou página e coloque um ponto de interrupção clicando no número da linha ao lado da declaração `return`.
5. Atualize a página para acionar o ponto de interrupção.
6. Verifique o painel "Scope" (Escopo) para ver as variáveis locais `a` e `b`.
7. Avance com o botão "Step over next function call" (Avançar sobre a próxima chamada de função).
8. Encontre o bug na declaração `return`.
9. Corrija o código:
```javascript
function buggyMultiply(a, b) {
    return a * b; // Corrigido!
}

let result = buggyMultiply(5, 3);
console.log('Resultado:', result);
```

A saída corrigida:
```
Resultado: 15
```

## Aprofundamento
O conceito de depuração existe desde os primeiros dias da computação — a lenda diz que começou quando uma mariposa foi encontrada em um computador na década de 1940! Hoje, depuradores de JavaScript como as ferramentas integradas ao navegador (Chrome DevTools, Ferramentas do Desenvolvedor do Firefox) ou depuradores integrados ao IDE (Visual Studio Code, WebStorm) oferecem uma tonelada de recursos.

Alternativas aos depuradores integrados incluem ferramentas de terceiros como o WebStorm ou usar o bom e velho `console.log` para exibir estados de variáveis. Mas estes não oferecem a interação em tempo real e inspeção detalhada fornecida pelos depuradores.

Quanto aos detalhes da implementação, a maioria dos depuradores funciona de maneira similar: eles permitem que você defina pontos de interrupção que pausam a execução, avance pelo código, inspecione os estados atuais das variáveis, observe expressões e até manipule valores na hora para testar diferentes cenários.

## Veja Também
- [Google Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools)
- [Mozilla Developer Network - Firefox Debugger](https://developer.mozilla.org/en-US/docs/Tools/Debugger)
- [Visual Studio Code - Depuração](https://code.visualstudio.com/docs/editor/debugging)

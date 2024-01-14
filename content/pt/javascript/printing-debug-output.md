---
title:                "Javascript: Imprimindo saída de depuração"
programming_language: "Javascript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

# Por que imprimir saída de depuração em Javascript?

Debugar código é uma parte essencial do processo de programação. No entanto, muitos programadores tendem a subestimar a importância de imprimir saída de depuração em seu código Javascript. Fazer isso pode fornecer informações valiosas sobre o fluxo do código e ajudar a identificar e corrigir erros de forma mais eficiente. Neste artigo, vamos explorar a importância da saída de depuração em Javascript e como implementá-la em seu código.

## Como fazer

A maneira mais simples de imprimir saída de depuração em Javascript é usando o comando `console.log()`. Vamos supor que temos a seguinte função que calcula a média de dois números:

```Javascript
function calcularMedia(a, b) {
  console.log(`Calculando a média de ${a} e ${b}.`);
  let media = (a + b) / 2;
  console.log(`A média é ${media}.`);
  return media;
}

calcularMedia(4, 6);
```

A saída de depuração deste código seria:

```
Calculando a média de 4 e 6.
A média é 5.
```

Isso nos permite ver os valores que estão sendo passados para a função e a média calculada, o que pode ser útil para identificar erros em nosso código. Além de `console.log()`, há também outros métodos de saída de depuração, como `console.info()`, `console.warn()` e `console.error()`, que podem ser usados para imprimir informações específicas sobre o estado do código.

## Mergulho profundo

Utilizar saída de depuração em Javascript também pode ser muito útil para entender o fluxo do código e realizar testes. Quando se tem um código complexo, pode ser difícil acompanhar o que está acontecendo em cada etapa do processo. Com a saída de depuração, podemos ver os valores das variáveis e como eles são modificados ao longo do código, o que ajuda a identificar possíveis problemas e otimizar o desempenho.

Além disso, podemos utilizar métodos como `console.table()` para imprimir dados estruturados em uma tabela, facilitando a visualização e compreensão dos dados.

# Veja também

- [Documentação oficial do console em Javascript](https://developer.mozilla.org/pt-BR/docs/Web/API/Console)
- [Artigo sobre debug em Javascript](https://tableless.com.br/o-guia-definitivo-de-debug-de-aplicacoes-javascript/)
- [Livro sobre depuração de código em Javascript](https://www.casadocodigo.com.br/products/livro-depurando-javascript)
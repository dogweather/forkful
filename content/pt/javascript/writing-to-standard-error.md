---
title:    "Javascript: Escrevendo para o erro padrão."
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

Escrever para o erro padrão é uma habilidade importante para qualquer programador, pois permite que você tenha um maior controle sobre o fluxo e o comportamento do seu código. Além disso, isso também pode ser uma ferramenta útil para identificar e solucionar erros em seu código.

## Como fazer

Escrever para o erro padrão em Javascript é bastante simples e pode ser feito de várias maneiras diferentes. Aqui estão alguns exemplos de como você pode utilizá-lo em seu código:

```Javascript
// Enviando uma mensagem para o erro padrão
console.error("Ocorreu um erro!");

// Capturando o erro padrão e exibindo-o no console
try {
  // código que pode gerar um erro
} catch (err) {
  console.error(err);
}

// Escrevendo para o erro padrão com o objeto Error
let err = new Error("Mensagem de erro");
console.error(err);
```
O console de saída mostrará a mensagem de erro na cor vermelha para chamar a atenção e identificar facilmente o problema.

## Profundidade

Agora vamos nos aprofundar um pouco mais em como o erro padrão funciona em Javascript. Essa funcionalidade é baseada no console de desenvolvedor, que é uma ferramenta para depuração de código no navegador. Ao usar `console.error`, você está enviando uma mensagem para esse console, que será exibida como um erro.

Você também pode usar a instrução `throw` para gerar um erro e enviar uma mensagem personalizada para o erro padrão, como mostrado no exemplo a seguir:

```Javascript
let num = 10;

if (num > 5) {
  throw new Error("O número deve ser menor que 5");
}
```

Isso irá gerar um erro e enviar a mensagem para o erro padrão, impedindo que o código continue a ser executado.

## Veja também

Aqui estão alguns recursos adicionais que podem ajudá-lo a entender melhor como escrever para o erro padrão em Javascript:

- [Documentação MDN sobre console.error](https://developer.mozilla.org/pt-BR/docs/Web/API/Console/error)
- [Artigo sobre como usar erros em Javascript](https://www.digitalocean.com/community/tutorials/how-to-use-errors-in-javascript)
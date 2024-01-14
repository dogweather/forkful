---
title:    "Javascript: Escrevendo para o erro padrão"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

Escrever para o erro padrão é uma prática essencial para os desenvolvedores de Javascript. Quando o código encontra um erro ou um comportamento inesperado, ele é imediatamente registrado no erro padrão, também conhecido como console de desenvolvedor. Isso permite que os programadores possam detectar e corrigir esses erros, tornando o código mais robusto e confiável.

## Como fazer

Para escrever para o erro padrão em Javascript, usamos o método "console.error()". Este método imprime uma mensagem de erro no console e destaca o erro, permitindo que o desenvolvedor o localize facilmente. Veja o exemplo abaixo:

```Javascript
console.error("Erro: Variável não definida");
```

Este código irá imprimir a mensagem "Erro: Variável não definida" no console e indicar a linha onde o erro ocorreu. O formato é simples e direto, facilitando a identificação dos erros no código.

## Mergulho profundo

Escrever para o erro padrão também pode ser útil quando queremos registrar informações sobre o desempenho do nosso código. Podemos criar mensagens personalizadas para indicar o tempo de execução de funções ou loops, por exemplo. Além disso, podemos usar o console para imprimir valores de variáveis e garantir que estão sendo atribuídos corretamente.

Outra funcionalidade importante é a possibilidade de passar mais de um argumento para o "console.error()". Isso permite que concatenemos mensagens e variáveis, facilitando a leitura e o entendimento do erro.

```Javascript
let nome = "Maria";
console.error("Erro: A variável " + nome + " não foi declarada corretamente");
```

Além disso, o console também nos permite colorir as mensagens, tornando-as mais visuais e ainda mais fáceis de serem identificadas no meio de linhas de código e outros registros.

Lembre-se de sempre remover os registros de erro do seu código antes de publicá-lo, para manter a segurança das informações. Utilize a ferramenta de inspeção do navegador para visualizar os erros sem a necessidade de deixá-los no código.

## Veja também

- Documentação do método "console.error()": https://developer.mozilla.org/pt-BR/docs/Web/API/Console/error
- Como usar a ferramenta de inspeção do navegador: https://developer.mozilla.org/pt-BR/docs/Tools/Page_Inspector/How_to/Examine_errors
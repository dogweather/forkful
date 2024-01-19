---
title:                "Comparando duas datas"
html_title:           "C#: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O quê & Por quê?

Comparar duas datas é o ato de verificar a diferença entre duas datas na programação. Programadores geralmente precisam disso para efetuar cálculos relacionados a tempo, como determinar a diferença entre duas datas, eventos programados, durações, dentre outras coisas.

## Como fazer:

```Javascript
// Criando duas datas
var data1 = new Date('2021-01-01');
var data2 = new Date('2022-01-01');

// Comparando as duas datas
if(data1.getTime()==data2.getTime()){
   console.log("As datas são iguais"); 
}
else if(data1.getTime()<data2.getTime()){
   console.log("A data1 é menor do que a data2"); 
}
else{
   console.log("A data1 é maior do que a data2");
}
```
A saída será: "A data1 é menor do que a data2"

## Deep Dive

Na verdade, dados os objetos de data no JavaScript, é possível comprar duas datas diretamente em vez de pegar sua representação em milissegundos, mas usar `getTime()` frequentemente proporciona desempenho melhor e nos assegura que estamos realmente comparando o valor temporal. 

As alternativas para comparar duas datas no JavaScript dependem do que você realmente precisa. Se você precisa apenas verificar se duas datas são iguais até o dia, você pode tornar as datas strings e então compará-las, ou ainda pode usar bibliotecas externas, como o Moment.js, que possui ampla variedade de funções para manipulação e comparação de datas.

Historicamente, o JavaScript teve algumas inconsistências no manuseio de datas, e ainda hoje é um ponto onde muitos programadores novatos e até mesmo experientes se confundem. Entender as peculiaridades das datas no JavaScript é um passo fundamental para criar programas robustos.

## Veja também:

MDN Web Docs for Date: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date

Moment.js: https://momentjs.com/

Stack Overflow thread "how to compare dates in JavaScript": https://stackoverflow.com/questions/492994/compare-two-dates-with-javascript

A compreensão do manuseio de datas em JavaScript: https://blog.logrocket.com/a-guide-to-dates-in-javascript/
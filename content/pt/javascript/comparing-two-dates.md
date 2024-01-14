---
title:                "Javascript: Comparando duas datas"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas em Javascript?

As datas são uma parte essencial da programação em Javascript, especialmente quando se trata de lidar com dados e informações em um projeto. Comparar datas em Javascript pode ser útil para verificar se uma determinada data é anterior ou posterior a outra, determinar a diferença entre duas datas e muito mais. É uma habilidade importante para dominar para qualquer desenvolvedor que trabalhe com datas em seus projetos.

## Como comparar duas datas em Javascript

Existem várias maneiras de comparar duas datas em Javascript, dependendo da precisão e do tipo de comparação que você deseja fazer. Aqui estão alguns exemplos de código que demonstram diferentes formas de realizar essa tarefa:

- Comparando o valor numérico das datas:
```Javascript
let data1 = new Date("2021-05-01");
let data2 = new Date("2021-06-01");
if (data1.valueOf() > data2.valueOf()) {
    console.log("A data1 é posterior à data2");
} else if (data1.valueOf() < data2.valueOf()) {
    console.log("A data1 é anterior à data2");
} else {
    console.log("As datas são iguais");
}
```
Saída: A data1 é anterior à data2

- Utilizando o método `getDate()`:
```Javascript
let data1 = new Date("2021-05-01");
let data2 = new Date("2021-05-15");
if (data1.getDate() > data2.getDate()) {
    console.log("A data1 é posterior à data2");
} else if (data1.getDate() < data2.getDate()) {
    console.log("A data1 é anterior à data2");
} else {
    console.log("As datas são iguais");
}
```
Saída: A data1 é anterior à data2

- Usando o objeto `Date()` para obter uma diferença em milissegundos:
```Javascript
let data1 = new Date("2021-05-01");
let data2 = new Date("2021-05-15");
let diff = data2 - data1;
console.log(diff);
```
Saída: 1209600000 (representando o número de milissegundos entre as duas datas)

## Aprofundando na comparação de datas

Quando se trabalha com datas em Javascript, é importante entender que elas são objetos e não simples valores. Portanto, ao comparar datas, você está comparando objetos e não apenas a informação que eles representam.

Além disso, lembre-se de que a data e a hora podem ser afetadas pelo fuso horário do usuário ou do servidor onde o código está sendo executado. Isso pode resultar em resultados inesperados ao comparar datas em diferentes lugares. Utilizar o método `getTimezoneOffset()` pode ajudar a resolver esse problema.

Outro ponto importante é que o objeto `Date` permite manipulações, como adicionar ou subtrair dias, horas, minutos e segundos. Isso pode ser útil em cenários em que você precisa comparar datas com uma certa margem de tolerância, em vez de uma comparação exata.

## Veja também 

- [Documentação oficial do objeto Date em Javascript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Tutorial sobre manipulação de datas em Javascript](https://www.digitalocean.com/community/tutorials/how-to-work-with-dates-in-javascript)
- [Exemplos de comparação de datas em Javascript](https://www.w3schools.com/js/js_dates.asp)

O conhecimento sobre como comparar datas em Javascript é fundamental para qualquer desenvolvedor que trabalhe com datas em seus projetos. Esperamos que este artigo tenha sido útil para expandir seu conhecimento sobre esse assunto. Agora é hora de aplicar esse conhecimento em seus próprios projetos e continuar aprendendo cada vez mais sobre a linguagem Javascript. Até a próxima!
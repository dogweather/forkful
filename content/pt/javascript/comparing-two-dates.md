---
title:    "Javascript: Comparando duas datas"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Por que comparar duas datas em Javascript?

A comparação de datas é um processo importante em qualquer linguagem de programação, incluindo Javascript. Ao comparar duas datas, podemos determinar a ordem cronológica entre elas e realizar ações com base nessa informação. Isso é especialmente útil ao lidar com dados temporais, como agendamentos de eventos ou validação de datas de entrada.

## Como comparar duas datas em Javascript

A comparação de datas em Javascript é feita usando o método `getTime()` dos objetos `Date`. Este método retorna o número de milissegundos desde 1º de janeiro de 1970, também conhecido como "época Unix". Podemos então comparar esses valores para determinar qual data é maior ou menor.

Veja um exemplo de código abaixo:

```Javascript
let data1 = new Date('2021-05-15');
let data2 = new Date('2021-06-01');

if(data1.getTime() < data2.getTime()) {
    console.log('A data 1 é anterior à data 2');
} else {
    console.log('A data 2 é anterior à data 1');
}
```

No exemplo acima, criamos duas datas diferentes usando a sintaxe `new Date()` e as comparamos usando o método `getTime()`. A data 1 é anterior à data 2, então o primeiro `console.log` será exibido no console.

## Mais detalhes sobre comparação de datas

Em Javascript, as datas também podem ser comparadas usando os operadores de comparação `>`, `<`, `>=` e `<=`. No entanto, é importante lembrar que esses operadores comparam as datas usando seus valores de string e não seus valores de tempo. Isso pode levar a resultados inesperados, portanto, é sempre recomendado usar o método `getTime()` para comparações precisas.

Também devemos ter cuidado ao comparar datas usando objetos `Date` diferentes. Isso ocorre porque, por padrão, os objetos `Date` são criados usando o fuso horário do sistema em que o código está sendo executado. Isso pode levar a uma diferença de alguns milissegundos entre as duas datas, o que pode afetar a ordem de comparação. Para evitar isso, podemos usar o método `setUTCHours()` para definir as horas em UTC antes da comparação.

## Veja também

- [Documentação oficial Javascript - Comparando Datas](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date#comparando_datas) 
- [Manipulando datas com Javascript](https://www.linhadecodigo.com.br/artigo/3376/manipulando-datas-com-javascript.aspx)
- [Comparação de datas em Javascript: Guia definitivo](https://codigofonte.com.br/artigos/comparar-datas-em-javascript)
---
title:    "Javascript: Comparando duas datas"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas em Javascript?

Comparar dois valores de data é uma tarefa comum em programação, especialmente em projetos que envolvem agendamento, rastreamento de tempo ou cálculos baseados em durações. É importante entender como a linguagem Javascript trata e compara dados de data para garantir resultados precisos e consistentes em seu código.

## Como comparar datas em Javascript

Em Javascript, as datas são armazenadas como objetos do tipo "Date", que representam um ponto específico no tempo. Para comparar duas datas, podemos usar os operadores de comparação, como ">", "<", ">=" ou "<=".

Por exemplo, se quisermos verificar se uma data é anterior a outra, usamos o operador "<":

```
var data1 = new Date("2021-01-01");
var data2 = new Date("2021-06-01");

if (data1 < data2) {
  console.log("A primeira data é anterior à segunda data.");
}
```

O console irá imprimir a mensagem "A primeira data é anterior à segunda data." porque 01 de janeiro é uma data anterior a 01 de junho.

Também é possível comparar as datas utilizando o método "getTime()", que retorna o valor numérico da data em milissegundos. Com isso, podemos comparar as datas com mais precisão, considerando não apenas o dia, mas também a hora, minuto, segundo e milissegundo.

```
var data1 = new Date("2021-01-01");
var data2 = new Date("2021-06-01");

if (data1.getTime() < data2.getTime()) {
  console.log("A primeira data é anterior à segunda data.");
}
```

O exemplo acima também imprimirá a mesma mensagem no console, pois o valor numérico de 01 de janeiro é menor que o de 01 de junho.

## Aprofundando na comparação de datas

É importante lembrar que as datas são objetos em Javascript e, portanto, podem ter suas propriedades e métodos. Quando comparamos datas, na verdade estamos comparando seus valores ou propriedades, que podem ser acessados utilizando os métodos "get" da data (ex: "getDate()", "getMonth()", "getFullYear()").

Além disso, quando comparamos datas com o operador "==" ou "!=", apenas os valores ou propriedades são comparados, não a referência do objeto. Portanto, duas datas com os mesmos valores, mas criadas com objetos diferentes, serão consideradas iguais.

```
var data1 = new Date("2021-01-01");
var data2 = new Date("2021-01-01");

if (data1 == data2) {
  console.log("As datas são iguais.");
}
```

O console imprimirá a mensagem "As datas são iguais." mesmo que as datas tenham sido criadas com objetos diferentes.

## Veja também

- [Documentação sobre objetos Date em Javascript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Como converter datas em diferentes formatos em Javascript](https://www.w3schools.com/js/js_date_formats.asp)
- [Comparando valores com os operadores de comparação em Javascript](https://www.w3schools.com/js/js_comparisons.asp)
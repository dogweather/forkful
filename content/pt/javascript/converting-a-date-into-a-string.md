---
title:                "Convertendo uma data em uma string"
html_title:           "Javascript: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Se você já se deparou com a necessidade de converter uma data em um formato legível por humanos, então você já deve ter se perguntado por que é necessário realizar essa conversão. A resposta é simples: a maioria das ferramentas e bibliotecas que manipulam datas utilizam formatos específicos e numéricos. Porém, muitas vezes precisamos exibir essas datas em um formato mais familiar e compreensível pelas pessoas, por isso é importante saber como realizar essa conversão.

## Como fazer

Para converter uma data em uma string utilizando Javascript, existem alguns métodos que podem ser utilizados. Abaixo estão alguns exemplos utilizando diferentes abordagens.

```javascript
// Opção 1: Utilizando o método .toLocaleString()
let data = new Date();
console.log(data.toLocaleString()); // Exemplo de saída: 13/10/2021, 14:30:00

// Opção 2: Utilizando o método .toString()
console.log(data.toString()); // Exemplo de saída: Wed Oct 13 2021 14:30:00 GMT-0300 (Horário Padrão de Brasília)

// Opção 3: Utilizando o método .toDateString()
console.log(data.toDateString()); // Exemplo de saída: Wed Oct 13 2021
```

Esses são apenas alguns exemplos de possíveis métodos de conversão de data em string. Cada um deles pode ser útil em diferentes situações e requisitos do projeto. Além disso, é possível utilizar formatação personalizada com alguns desses métodos, que são bem documentados na documentação oficial do Javascript.

## Mergulho profundo

Por trás da conversão de datas em Javascript, existem conceitos importantes a serem compreendidos. Primeiramente, é necessário entender o conceito de timestamp, que é a representação numérica da data e hora em segundos desde o início da chamada "era Unix". A conversão entre timestamp e datas é feita através do método .getTime(), que retorna o timestamp em milissegundos.

Outro conceito importante é o de Time Zone, que é a diferença de horário entre diferentes regiões geográficas do mundo. É importante levar em consideração o Time Zone ao trabalhar com datas, pois pode afetar a exibição correta da data.

A conversão de uma data em uma string também pode ser realizada com diferentes formatações, como por exemplo, exibir apenas a data ou apenas a hora, exibir a data no formato meses/dias/anos ou dias/meses/anos, entre outros. É importante considerar esses fatores ao decidir qual método utilizar para realizar a conversão.

## Veja também

- Documentação oficial do Javascript sobre conversão de datas: <https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date>
- Artigo sobre Time Zone: <https://medium.com/@evertoncorreia/javascript-o-que-%C3%A9-timezone-e-como-trabalhar-com-date-e-timezone-b0b58c6a027b>
---
date: 2024-01-20 17:33:16.539600-07:00
description: "How to: Comparar datas em JavaScript tem sido um t\xF3pico importante\
  \ desde o princ\xEDpio do desenvolvimento web. Historicamente, bibliotecas como\
  \ Moment.js\u2026"
lastmod: '2024-04-05T21:53:47.326181-06:00'
model: gpt-4-1106-preview
summary: "Comparar datas em JavaScript tem sido um t\xF3pico importante desde o princ\xED\
  pio do desenvolvimento web."
title: Comparando duas datas
weight: 27
---

## How to:
```Javascript
// Criar duas datas
let data1 = new Date('2023-05-15T00:00:00');
let data2 = new Date('2023-05-20T00:00:00');

// Comparar datas (data1 é antes de data2?)
console.log(data1 < data2); // Saída: true

// Comparar datas (data1 é depois de data2?)
console.log(data1 > data2); // Saída: false

// São exatamente iguais? (mesmo momento no tempo)
console.log(data1.getTime() === data2.getTime()); // Saída: false
```

## Deep Dive
Comparar datas em JavaScript tem sido um tópico importante desde o princípio do desenvolvimento web. Historicamente, bibliotecas como Moment.js eram a solução pra muitos, mas com novas especificações do ECMAScript e melhorias nos browsers, muitos desses recursos são embutidos. `Date` é um objeto built-in que representa uma única data e hora. Podemos comparar dois objetos `Date` convertendo-os em timestamps usando `getTime()`, que retorna o valor numérico correspondente ao tempo daquela data desde 1 de janeiro de 1970 UTC. Esta abordagem evita falsos negativos de comparações diretas, que podem acontecer devido a diferenças nos milissegundos.

Alternativas modernas incluem a nova API Temporal proposta para uma melhor manipulação e formatação de datas em JavaScript, tratando de muitas limitações do objeto `Date`.

Para detalhes de implementação, é crucial entender o funcionamento do timezone que pode afetar comparações quando não manuseamos devidamente.

## See Also
- [MDN Web Docs sobre Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Temporal API Introduction](https://tc39.es/proposal-temporal/docs/index.html)
- [ISO 8601 Data elements and interchange formats – Information interchange – Representation of dates and times](https://www.iso.org/iso-8601-date-and-time-format.html)

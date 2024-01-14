---
title:    "TypeScript: Calculando uma data no futuro ou no passado."
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por que

Calcular datas futuras ou passadas pode ser útil em muitos cenários de programação, como planejar eventos ou notificar usuários sobre prazos. Além disso, é uma habilidade importante para qualquer programador ter. Neste post, vamos aprender como calcular datas em TypeScript e mergulhar um pouco mais fundo nesse assunto.

## Como fazer

Usando TypeScript, podemos calcular datas em um formato bem simples e direto. Primeiro, precisaremos importar a biblioteca `moment.js` que nos permite manipular e formatar datas. Depois, podemos usar o método `add()` para adicionar ou subtrair um determinado número de dias, meses ou anos a partir de uma data específica. Veja o exemplo abaixo:

```TypeScript
import * as moment from 'moment';

// calcula uma data 7 dias no futuro
let data = moment().add(7, 'days');

console.log(data.format('DD/MM/YYYY')); // saída: 31/01/2022

// calcula uma data 2 meses no passado
let outraData = moment('25/03/2022', 'DD/MM/YYYY').add(-2, 'months');

console.log(outraData.format('DD/MM/YYYY')); // saída: 25/01/2022
```

Nesse exemplo, usamos o método `.format()` para exibir a data em um formato específico. Também podemos usar outros métodos, como `subtract()` para subtrair datas e `diff()` para calcular a diferença entre duas datas.

## Deep Dive

Além dos métodos mencionados acima, a biblioteca `moment.js` também oferece várias outras funções úteis para facilitar o cálculo de datas. Alguns exemplos incluem `startOf()` e `endOf()` para obter o início e o fim de um determinado período de tempo, `isLeapYear()` para verificar se um ano é bissexto e `isBetween()` para verificar se uma data está dentro de um determinado intervalo.

É importante lembrar que as datas são sensíveis ao fuso horário em que estão sendo calculadas. Portanto, é sempre importante especificar o fuso horário correto ao trabalhar com datas para evitar erros de cálculo.

## Veja também

- [Documentação oficial do Moment.js](https://momentjs.com/)
- [Tutorial sobre manipulação de datas em JavaScript](https://www.digitalocean.com/community/tutorials/understanding-date-and-time-in-javascript)
- [Repositório do Moment.js no GitHub](https://github.com/moment/moment/)
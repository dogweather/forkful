---
aliases:
- /pt/javascript/calculating-a-date-in-the-future-or-past/
date: 2024-01-20 17:31:29.301278-07:00
description: "Calcular datas no futuro ou no passado significa ajustar um ponto de\
  \ partida temporal para obter uma nova data. Programadores realizam este processo\
  \ para\u2026"
lastmod: 2024-02-18 23:08:58.545368
model: gpt-4-1106-preview
summary: "Calcular datas no futuro ou no passado significa ajustar um ponto de partida\
  \ temporal para obter uma nova data. Programadores realizam este processo para\u2026"
title: Calculando uma data no futuro ou passado
---

{{< edit_this_page >}}

## O Quê & Porquê?
Calcular datas no futuro ou no passado significa ajustar um ponto de partida temporal para obter uma nova data. Programadores realizam este processo para manipular prazos, agendamentos ou para funções de lembrete.

## Como Fazer:
```Javascript
// Adicionar dias à data atual
const hoje = new Date();
const diasParaAdicionar = 10;
const dataFutura = new Date(hoje.getTime() + diasParaAdicionar * 24 * 60 * 60 * 1000);
console.log(dataFutura);

// Subtrair dias da data atual
const diasParaSubtrair = 5;
const dataPassada = new Date(hoje.getTime() - diasParaSubtrair * 24 * 60 * 60 * 1000);
console.log(dataPassada);
```
Saída de exemplo:
```
// Saída possível se hoje for 1 de Abril de 2023
2023-04-11T12:35:00.000Z // dataFutura
2023-03-27T12:35:00.000Z // dataPassada
```

## Mais Detalhes:
Historicamente, a gestão de datas em JavaScript foi algo complicado devido às diferenças de fuso horário e ao tratamento ineficiente do objeto `Date`. Hoje, existem bibliotecas como Moment.js e date-fns que simplificam essas operações, mas sempre é útil saber manipular o objeto `Date` nativo do JavaScript. 

Para calcular uma data futura ou passada, o trabalho envolve obter o timestamp atual, que é o número de milissegundos desde o Unix Epoch (1º de Janeiro de 1970), e adicionar ou subtrair o número equivalente de milissegundos para o intervalo de tempo desejado. Depois, cria-se uma nova data com o novo timestamp.

Ao trabalhar com datas, é importante considerar o fuso horário do usuário e a possibilidade de horário de verão. Em ambientes de produção, recomendam-se bibliotecas especializadas que lidam com essas complexidades.

## Veja Também:
- [MDN Web Docs - Date](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [date-fns](https://date-fns.org/)
- [You Don't Need Moment.js](https://github.com/you-dont-need/You-Dont-Need-Momentjs)

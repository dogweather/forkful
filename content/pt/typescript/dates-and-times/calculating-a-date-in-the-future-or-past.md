---
title:                "Calculando uma data no futuro ou passado"
aliases:
- /pt/typescript/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:32:08.930809-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calculando uma data no futuro ou passado"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Calcular datas futuras ou passadas é um jeito de lidar com intervalos de tempo em programação. Programadores fazem isso para agendar eventos, fazer relatórios ou verificar a validade de alguma informação com base no tempo.

## Como Fazer:
```TypeScript
const hoje: Date = new Date();
const diasParaAdicionar: number = 10;

// Acrescentando dias à data atual para obter uma data no futuro.
const futuro: Date = new Date(hoje.getTime() + (diasParaAdicionar * 24 * 60 * 60 * 1000));
console.log(`Data Futura: ${futuro.toLocaleDateString()}`);

const diasParaSubtrair: number = 5;

// Subtraindo dias da data atual para obter uma data no passado.
const passado: Date = new Date(hoje.getTime() - (diasParaSubtrair * 24 * 60 * 60 * 1000));
console.log(`Data Passada: ${passado.toLocaleDateString()}`);
```
Sample Output:
```
Data Futura: 02/04/2023
Data Passada: 18/03/2023
```

## Análise Detalhada:
Datas são cruciais para quase todos os sistemas. A capacidade de manipular o tempo é herdada das linguagens C e C++, que influenciaram muito o JavaScript e, por extensão, o TypeScript. TypeScript, sendo um superset de JavaScript, lida com datas usando o mesmo objeto `Date` do JavaScript.

Outras bibliotecas como `moment.js` já foram muito usadas para manipulação mais complexa de datas por causa da simplicidade e poderosos recursos. No entanto, muitos estão migrando para bibliotecas mais modernas como `date-fns` ou `Day.js` devido às suas API mais leves e modularização melhor.

Quando se calcula datas, é importante considerar fusos horários e horário de verão. TypeScript não tem funcionalidades próprias para isso, então geralmente dependemos de APIs de navegador ou de bibliotecas externas.

## Veja Também:
- MDN Web Docs - Date: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date
- Day.js: https://day.js.org/
- date-fns: https://date-fns.org/
- Luxon: https://moment.github.io/luxon/#/?id=luxon

---
title:                "Obtendo a data atual"
date:                  2024-01-20T15:15:14.921716-07:00
simple_title:         "Obtendo a data atual"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Obter a data atual em Javascript significa acessar o momento exato em que a linha de código é executada. Programadores fazem isso para funcionalidades como logs de atividades, datas de criação de conteúdo ou simplesmente para mostrar o dia corrente numa página web.

## Como Fazer:
```Javascript
// Obtém a data e hora atual
const agora = new Date();

// Exibe no console
console.log(agora.toString()); // Exemplo de saída: Wed Mar 10 2021 15:50:00 GMT+0000 (Horário Coordenado Universal)

// Formata para uma apresentação mais legível em pt-BR
const opcoes = { day: '2-digit', month: '2-digit', year: 'numeric' };
console.log(agora.toLocaleDateString('pt-BR', opcoes)); // Exemplo de saída: 10/03/2021
```

## Mergulho Profundo
`Date` é um objeto embutido em Javascript desde a sua primeira versão. Ele armazena a data e hora baseado no tempo do sistema e no fuso horário do navegador. Possíveis alternativas incluem bibliotecas como Moment.js ou Date-fns para manipulações de data mais complexas e robustas. Apesar de acréscimos periódicos de métodos ao objeto Date, as operações básicas mantêm-se inalteradas. O desafio ao usar o Date é lidar com fusos horários e horário de verão, que podem causar inconsistências.

## Veja Também
- MDN Web Docs sobre o objeto Date: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date
- Documentação do Moment.js: https://momentjs.com/docs/
- Documentação do Date-fns: https://date-fns.org/docs/Getting-Started
- Artigo sobre fusos horários e horário de verão: https://www.iana.org/time-zones

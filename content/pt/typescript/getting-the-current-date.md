---
title:                "Obtendo a data atual"
html_title:           "C: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Obter a data atual significa acessar a data e hora atuais conforme informado pelo sistema. É útil quando um programador precisa registrar quando um evento ocorreu ou usar a data e hora em cálculos ou condicionais.

## Como fazer:
No TypeScript, você pode obter a data e hora atuais usando o objeto Date. Veja um exemplo simples:

```TypeScript
let agora = new Date();
console.log(agora);
```

Este exemplo irá imprimir a data e a hora atuais no console.

## Aprofundando um pouco
O objeto Date do JavaScript (e, por extensão, o TypeScript) tem suas raízes nos primórdios do Netscape Navigator. Embora existam algumas bibliotecas alternativas lá fora que oferecem mais funcionalidades (como moment.js ou date-fns), o objeto Date ainda é uma opção sólida para a maioria dos casos de uso.

Quando você cria um novo objeto Date sem argumentos, o JavaScript o preenche com a data e hora atuais do sistema. Ele retorna os valores como uma string no horário local. Se quiser trabalhar com fusos horários diferentes, provavelmente precisará de uma biblioteca como o moment.js, que oferece suporte mais sólido para fusos.

## Veja também:
- [MDN Web Docs: Date](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date/)
- [date-fns.org](https://date-fns.org/)
- [momentjs.com](https://momentjs.com/)
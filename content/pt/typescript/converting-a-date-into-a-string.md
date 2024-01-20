---
title:                "Convertendo uma data em uma string"
html_title:           "C++: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O quê & por quê?

Traduzir uma data para string é um processo que permite representar datas como textos de fácil leitura para humanos ou para fins de armazenamento. Programadores fazem isso para facilitar o processamento, a visualização ou o armazenamento de datas.

## Como fazer:

No TypeScript, temos várias maneiras de converter uma data em uma string. Vamos usar o objeto Date e seus métodos.

```TypeScript
let agora = new Date();
console.log(agora.toString()); // "Fri Jun 18 2021 12:20:18 GMT+0100 (Horário de Verão da Europa Ocidental)"
console.log(agora.toISOString()); // "2021-06-18T11:20:18.290Z"
```

O `toString()` devolve a data e a hora no formato de string humano, enquanto o `toISOString()` retorna um string no formato ISO 8601, que é sempre de 24 caracteres.

## Mergulho profundo

Como alternativa, podemos usar `toLocaleString()`, `toLocaleDateString()` e `toLocaleTimeString()`. Eles permitem formatação mais regional:

```TypeScript
console.log(agora.toLocaleString()); // "18/06/2021, 12:20:18"
console.log(agora.toLocaleDateString()); // "18/06/2021"
console.log(agora.toLocaleTimeString()); // "12:20:18"
```

Historicamente, lidar com datas na programação tem sido notoriamente problemático devido a questões como fusos horários e a variedade de formatos de data. Por isso, existem bibliotecas como o Moment.js que fornecem funções robustas para manipulação de datas e horas.

Use a abordagem que melhor atenda às necessidades do seu projeto, seja para internacionalização, armazenamento eficiente de espaço ou compatibilidade com outras tecnologias.

## Veja também

- Documentação oficial do JavaScript [Date](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date) na MDN Web Docs
- Library [Moment.js](https://momentjs.com/) para manipulação de datas e horas
---
title:                "TypeScript: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual?

Alguns projetos de programação exigem que a data atual seja obtida. Isso pode ser útil para acompanhar o tempo entre transações, rastreamento de dados históricos e muito mais. Felizmente, com TypeScript, obter a data atual é simples e direto!

## Como fazer

Para obter a data atual em TypeScript, podemos usar a classe `Date`. Esta classe fornece métodos para recuperar a data atual, bem como para manipular datas e horários.

```TypeScript
// Criando uma nova instância da classe Date
const dataAtual = new Date();

// Obtendo a data atual em formato de string
const data = dataAtual.toDateString();
console.log(data); //Output: Sat Aug 14 2021

// Obtendo o dia atual
const dia = dataAtual.getDay();
console.log(dia); //Output: 6 (representando sábado)

// Obtendo o mês atual
const mes = dataAtual.getMonth();
console.log(mes); //Output: 7 (representando agosto)

// Obtendo o ano atual
const ano = dataAtual.getFullYear();
console.log(ano); //Output: 2021
```
Podemos ver que, ao chamar os métodos correspondentes, podemos obter facilmente diferentes informações sobre a data atual.

## Mais detalhes

Além dos métodos mencionados acima, a classe `Date` em TypeScript oferece várias outras opções para obter informações sobre a data atual e para manipular datas e horários de forma mais avançada. Por exemplo, podemos verificar se um ano é bissexto ou obter um horário específico em um dia específico.

É importante lembrar que a classe `Date` trabalha com o horário local, portanto, os resultados podem variar dependendo do fuso horário definido no sistema operacional.

## Veja também

- Documentação oficial do TypeScript para a classe `Date`: https://www.typescriptlang.org/docs/handbook/utility-types.html#date
- Tutorial de data e hora em TypeScript: https://www.tutorialspoint.com/typescript/typescript_date.htm
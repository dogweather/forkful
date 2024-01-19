---
title:                "Comparando duas datas"
html_title:           "C#: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O que é & Por quê?

Comparar duas datas significa verificar se uma data é maior, menor ou igual à outra. Programadores fazem isso para manipular informações baseadas em tempo real ou histórico, como classificar eventos em ordem cronológica ou calcular a diferença de tempo entre duas datas.

## Como fazer:

Comparando duas datas em TypeScript é bastante fácil. Aqui estão alguns exemplos de códigos:

```TypeScript
let data1 = new Date(2023, 1, 1);
let data2 = new Date(2022, 1, 1);

// Use '>' para verificar se a data1 é posterior à data2
if (data1 > data2) {
    console.log("Data 1 é mais recente");
} else if (data1 < data2) {
    console.log("Data 2 é mais recente");
} else {
    console.log("As datas são iguais");
}
// Saída: Data 1 é mais recente
```

## Mergulho profundo

Historicamente, a comparação de datas em JavaScript (e, consequentemente, TypeScript) se baseia na representação numérica de uma data como o número de milissegundos desde a meia-noite de 1 de Janeiro de 1970, também conhecido como 'epoch time'.

Como alternativa, você também pode usar bibliotecas externas, como moment.js ou dayjs, que proporcionam mais ferramentas para manipular e comparar datas. 

Com relação à implementação, vale ressaltar que ao comparar datas, preste atenção para garantir que está comparando as mesmas partes da data (por exemplo, apenas o dia e o mês, em vez do dia, mês e ano).

## Veja também

Para mais detalhes sobre a manipulação e comparação de datas, sugiro que você consulte os seguintes links:

- [Documentação do Mozilla Developer Network sobre Date](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Biblioteca Moment.js](https://momentjs.com/)
- [Biblioteca Day.js](https://day.js.org/)
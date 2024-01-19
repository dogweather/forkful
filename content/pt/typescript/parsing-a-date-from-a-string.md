---
title:                "Analisando uma data a partir de uma string"
html_title:           "PowerShell: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Entendendo a Análise de Datas em Strings no TypeScript

## O Que é e Por Que é importante?
A análise de uma data a partir de uma string envolve a transformação da representação de uma data em texto para um objeto data legível por máquina. Os programadores fazem isso para permitir uma manipulação de data mais fácil e eficiente em seus códigos.

## Como Fazer: 
Aqui está a maneira mais direta de analisar uma data a partir de uma string no TypeScript:

```TypeScript
let dataString: string = '2021-07-06';
let dataObjeto: Date = new Date(dataString);
console.log(dataObjeto);
```

Este código transformará a string `'2021-07-06'` em um objeto de data, produzindo a seguinte saída:

```TypeScript
2021-07-06T00:00:00.000Z
```

## Mergulhando mais fundo
Historicamente, a análise de datas a partir de strings tem sido um ponto de dor para os programadores, principalmente devido à falta de consistência nas formas como as datas são representadas em todo o mundo. TypeScript oferece alívio, fornecendo diversas maneiras de analisar datas, direta ou indiretamente.

Uma alternativa para analisar uma data a partir de uma string seria usar uma biblioteca like Moment.js, que oferece mais flexibilidade para lidar com datas e horários.

No TypeScript, o construtor Date pode ser usado para analisar strings de data, mas vale a pena notar que depende da compatibilidade do navegador e da implementação existe variação, e é por isso que bibliotecas como Moment.js são freqüentemente preferidas.

## Veja Também
Para explorar mais sobre tipos de dados e manipulações de datas em TypeScript, veja estes recursos úteis:
- [TypeScript Date Object](https://www.typescriptlang.org/docs/handbook/basic-types.html#about-number-string-boolean-and-object)
- [Momentjs Documentation](https://momentjs.com/docs/)
- [Date Parsing in JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/parse)
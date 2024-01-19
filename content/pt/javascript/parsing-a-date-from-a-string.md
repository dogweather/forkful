---
title:                "Analisando uma data a partir de uma string"
html_title:           "PowerShell: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Analisar uma data de uma string significa converter um texto formatado como data em um objeto Date em JavaScript. Os programadores fazem isso para manipular e comparar datas de maneira eficiente.

## Como fazer:

O uso mais comum é com o construtor `Date` nativo. Veja um exemplo:

```Javascript
let stringData = "2021-11-23T04:56:57.000Z";
let data = new Date(stringData);
console.log(data);
// Saída: 2021-11-23T04:56:57.000Z
```

Outra abordagem é utilizar a biblioteca `moment.js`:

```Javascript
var moment = require('moment');
var stringData = "23/11/2021";
var data = moment(stringData, "DD/MM/YYYY");
console.log(data);
```

## Exploração Aprofundada:

1. **Contexto histórico:** O método `Date.parse` existe desde as primeiras versões do JavaScript, mas sua implementação tem sido confusa e inconsistente entre navegadores.

2. **Alternativa:** Como vimos acima, `moment.js` é amplamente utilizado pelos programadores para evitar inconsistências entre diferentes implementações de `Date.parse`.

3. **Detalhes de implementação:** Em muitos casos, você terá que ajustar o código dependendo do formato específico da string da data.

## Veja Também:

A documentação oficial do JavaScript fornece mais detalhes sobre a classe Date:
https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date

Para mais detalhes sobre a biblioteca `moment.js`, confira a documentação oficial:
https://momentjs.com/

Aprenda mais sobre as diferenças na implementação do `Date.parse` entre navegadores diferentes:
https://stackoverflow.com/questions/5324178/javascript-date-parsing-on-iphone
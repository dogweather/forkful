---
title:    "Javascript: Calculando uma data no futuro ou no passado"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Calcular uma data no futuro ou passado é uma habilidade útil e necessária em muitos projetos de programação. Com a capacidade de manipular datas, os desenvolvedores podem obter informações valiosas e criar aplicações que lidam com tarefas relacionadas ao tempo.

## Como

Para calcular uma data no futuro ou passado, primeiro precisamos ter uma data inicial para trabalhar. Isso pode ser obtido usando o objeto Date do Javascript, que representa um único momento no tempo. Podemos definir uma data específica usando a seguinte sintaxe:

``` Javascript
// Definindo a data inicial para 1 de janeiro de 2022
let data = new Date(2022, 0, 1); //O primeiro parâmetro é o ano, o segundo é o mês (começando em zero) e o terceiro é o dia

// Função para adicionar dias a uma data
function adicionarDias(dataInicial, dias) {
    let dataFinal = new Date(); // Começamos com a data atual como ponto de partida
    dataFinal.setDate(dataInicial.getDate() + dias); // Adicionamos os dias desejados à data inicial
    return dataFinal; // Retorna a data final
}

console.log(adicionarDias(data, 10)); // Saída: Wed Jan 11 2022 00:00:00 GMT-0500 (Eastern Standard Time)
```

## Deep Dive

Ao calcular uma data no futuro ou passado, é importante ter em mente que o Javascript lida com datas usando o fuso horário local do computador. Isso pode causar problemas quando se trabalha com datas em diferentes fusos horários. Para evitar isso, é recomendável usar bibliotecas externas, como o Moment.js, que oferece uma ampla gama de funcionalidades para manipulação de datas.

Outro ponto importante a ser considerado ao manipular datas é o fato de que o objeto Date pode ser sensível ao ano bissexto. Para garantir resultados precisos, é importante levar em conta esse detalhe ao realizar cálculos com datas.

## Veja também

- [Documentação do objeto Date do Javascript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [Lidando com datas no Javascript - tutorial em vídeo em português](https://www.youtube.com/watch?v=Q7UEkFNO6Ao)
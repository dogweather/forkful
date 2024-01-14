---
title:                "Javascript: Obtendo a data atual"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que usar Javascript para obter a data atual? 

Existem várias razões para usar Javascript para obter a data atual. 
Primeiro, porque é uma linguagem de programação amplamente adotada e usada em muitas aplicações web. 
Além disso, a obtenção da data atual é uma tarefa comum em muitos projetos, seja para fins de registro ou para exibir informações atualizadas para os usuários.

## Como fazer isso: 

Usando a função integrada `Date()` em Javascript, podemos obter a data e hora atuais. 
Veja um exemplo abaixo que retorna a data atual em formato de string: 

```Javascript
const dataAtual = new Date();
console.log(dataAtual.toDateString());
```

O resultado no console seria algo como: 

> "Fri Oct 08 2021"

Podemos obter a data em outros formatos também, como incluindo o ano: 

```Javascript
const dataAtual = new Date();
console.log(dataAtual.getFullYear());
```

Que resultaria em: 

> 2021

## Mergulho Profundo: 

Embora a função `Date()` seja simples e eficaz para obter a data atual, é importante lembrar que ela utiliza o fuso horário do sistema em que está sendo executada. 
Portanto, se você deseja ter um controle mais preciso sobre a data e hora, é recomendável utilizar uma biblioteca como o [Moment.js](https://momentjs.com/).

Outro aspecto importante a ser considerado ao obter a data atual é a perfomance. 
Em casos em que a obtenção frequente da data é necessária, é importante estar atento ao impacto que isso pode ter no desempenho de sua aplicação.

## Veja também: 
- [Documentação MDN para a função Date()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [Artigo sobre melhorias no desempenho ao utilizar a função Date()](https://bocoup.com/blog/javascript-performance-getting-the-current-year)
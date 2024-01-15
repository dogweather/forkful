---
title:                "Obtendo a data atual"
html_title:           "TypeScript: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que

Não importa qual é o seu projeto, em algum momento você precisará trabalhar com datas. Seja para realizar uma ação programada ou para exibi-la em formato legível para o usuário, obter a data atual é uma tarefa frequente na programação. Felizmente, o TypeScript oferece uma maneira fácil e eficiente de fazer isso.

## Como Fazer

Para obter a data atual em TypeScript, basta utilizar a classe Date, que é nativa da linguagem. Veja um exemplo de código:

```TypeScript
const date = new Date(); // cria uma instância da classe Date para a data atual 
console.log(date); // exibe a data no formato padrão YYYY-MM-DDTHH:mm:ss.sssZ

// Saída: 2021-09-08T14:24:37.594Z
```

Você também pode formatar a data de acordo com as suas necessidades, utilizando os métodos disponíveis na classe Date. Por exemplo, para exibir a data no formato "DD/MM/YYYY", você pode utilizar os métodos getDate(), getMonth() e getFullYear():

```TypeScript
const date = new Date();
const day = date.getDate(); // obtém o dia atual
const month = date.getMonth() + 1; // o retorno do getMonth() inicia em 0, por isso é necessário adicionar 1
const year = date.getFullYear(); // obtém o ano atual

console.log(`${day}/${month}/${year}`); // exibe a data no formato desejado

// Saída: 08/09/2021
```

Também é possível adicionar ou subtrair dias, meses e anos a partir da data atual, utilizando os métodos setDate(), setMonth() e setFullYear(). Veja um exemplo:

```TypeScript
const date = new Date();
date.setFullYear(date.getFullYear() + 1); // adiciona 1 ano à data atual 
console.log(date); // exibe a data com o novo ano adicionado

// Saída: 2022-09-08T14:24:37.594Z
```

## Deep Dive

A classe Date do TypeScript é baseada na classe Date do JavaScript. Diferente de outras linguagens de programação, como o Java, que possuem classes específicas para trabalhar com datas, o TypeScript utiliza a classe Date nativa do JavaScript. Isso significa que muitos dos métodos e propriedades disponíveis na classe Date do JavaScript também estão disponíveis no TypeScript.

Uma das propriedades mais úteis da classe Date é o getTime(), que retorna o número de milissegundos desde 1º de janeiro de 1970. Com isso, é possível comparar datas e calcular a diferença entre elas. Veja um exemplo:

```TypeScript
const date1 = new Date("2021-09-09");
const date2 = new Date("2021-09-08");

const difference = date1.getTime() - date2.getTime(); // calcula a diferença em milissegundos entre as duas datas
console.log(difference); 

// Saída: 86400000 (diferença de 1 dia em milissegundos)
```

Outro método útil é o toLocaleString(), que permite exibir a data em formato localizado, de acordo com a linguagem do dispositivo em que o código está sendo executado. Por exemplo:

```TypeScript
const date = new Date();
console.log(date.toLocaleString("pt-BR")); // exibe a data no formato padrão brasileiro

// Saída: 08/09/2021 11:24:37
```
## Veja Também

Para saber mais sobre a classe Date do TypeScript, você pode consultar a documentação oficial da linguagem e também aprender sobre outras formas de trabalhar com datas em TypeScript. Aqui estão alguns recursos úteis:

- Documentação Oficial do TypeScript: https://www.typescriptlang.org/docs/handbook/classes.html#classes
- Manipulando datas com Moment.js em TypeScript: https://blog.logrocket.com/dates-typescript-know-to-solve-problems-native-js/
- Diferenças entre Date do JavaScript e do TypeScript: https://stackoverflow.com/questions/51721844/how-is-the-typescript-date-class-different-from-javascripts#51721996
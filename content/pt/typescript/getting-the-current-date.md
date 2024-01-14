---
title:    "TypeScript: Obtendo a data atual"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Por que utilizar TypeScript para obter a data atual?

Ter a capacidade de obter a data atual é uma tarefa muito importante na programação. Isso pode ser útil para exibir informações em tempo real, registrar quando uma ação foi realizada ou até mesmo para fins de organização.

## Como fazer

Para obter a data atual em TypeScript, primeiro precisamos importar a biblioteca nativa do JavaScript para lidar com datas, chamada `Date`. Em seguida, vamos criar uma variável que vai guardar a data atual, utilizando o construtor `new` da classe Date.

```
TypeScript
importa a classe Date de 'javascript-native'

//Cria uma variável para guardar a data atual
let dataAtual = new Date();
```

Agora que temos a variável `dataAtual` criada, podemos acessar seus métodos para obter informações mais específicas. Por exemplo, se quisermos obter apenas o ano, podemos utilizar o método `.getFullYear()` da seguinte forma:

```
TypeScript
DataAtual.getFullYear(); // Retorna o ano atual
```

Outros métodos que podemos utilizar são `.getMonth()` para o mês atual, `.getDate()` para o dia atual e assim por diante. Também é possível formatar a data utilizando métodos como `.toLocaleDateString()`, que retorna a data no formato local do dispositivo.

## Profundidade

Existem alguns detalhes importantes a serem considerados ao obter a data atual utilizando TypeScript. Primeiramente, o objeto Date aceita um parâmetro opcional para especificar uma data específica, caso necessário. Além disso, é possível realizar operações matemáticas com datas, como adicionar ou subtrair dias, meses e anos utilizando os métodos `.setDate()`, `.setMonth()` e `.setFullYear()`.

Outra dica importante é que o objeto Date trabalha com o fuso horário do navegador, o que pode levar a resultados diferentes em diferentes partes do mundo.

# Veja também

- [Documentação da classe Date em JavaScript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Mais informações sobre formatação de datas em TypeScript](https://www.typescriptlang.org/docs/handbook/internationalization.html#formatting-dates)
- [Tutoriais sobre TypeScript no Youtube](https://www.youtube.com/results?search_query=typescript+pt)
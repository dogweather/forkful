---
title:    "TypeScript: Convertendo uma data em uma string"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Ao trabalhar com programação TypeScript, pode haver situações em que precisamos converter uma data em uma string. Isso pode ser necessário para exibir a data em um formato mais amigável ao usuário ou para armazená-la em um banco de dados. Felizmente, há maneiras simples e eficientes de realizar essa tarefa. 

## Como fazer:

Usando o método `toLocaleString()` podemos facilmente converter uma data em uma string com o formato desejado. Vamos ver um exemplo simples:

````TypeScript
const data = new Date();
const dataString = data.toLocaleString("pt-BR");
console.log(dataString);
````
Output: "21/09/2021 15:30:00"

No exemplo acima, criamos uma nova instância de `Date()` para a data atual e, em seguida, usamos o método `toLocaleString()` passando como parâmetro o código do idioma da formatação desejada. Isso resulta em uma string com o formato da data específico do idioma utilizado, no caso, o português do Brasil.

Se quisermos um controle maior sobre o formato da data, podemos usar o método `toLocaleDateString()`. Dessa forma, podemos especificar exatamente quais informações queremos incluir na string de data:

````TypeScript
const data = new Date();
const dataString = data.toLocaleDateString("pt-BR", {
    year: "numeric",
    month: "long",
    day: "numeric"
});
console.log(dataString);
````
Output: "21 de setembro de 2021"

Neste exemplo, especificamos que queremos a data com o mês escrito por extenso e o ano no formato numérico.

## Deep Dive:

Ao usar o método `toLocaleString()` para converter datas, também podemos fornecer opções adicionais para personalizar o formato da string. Podemos especificar o estilo da hora (12 ou 24 horas), o uso ou não da hora/minutos/segundos, adicionar informações como o fuso horário, entre outros.

Além disso, também é importante mencionar a importância de se definir corretamente o idioma ao usar esse método. Isso garante que a data será formatada de acordo com as convenções daquele idioma, incluindo o formato de data e a ordem dos campos.

## Veja também:

- [Guia de referência do método `toLocaleString()` no MDN](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString)
- [Documentação oficial do TypeScript sobre uso de datas](https://www.typescriptlang.org/docs/handbook/dates-and-times.html)
- [Uma introdução ao TypeScript para iniciantes](https://medium.com/@pchambino/typescript-um-guia-r%C3%A1pido-para-iniciantes-428e5ab6e51)
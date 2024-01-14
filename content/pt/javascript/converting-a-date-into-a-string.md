---
title:    "Javascript: Convertendo uma data em uma string"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que Converter uma Data em uma String?

Ao lidar com datas em um programa JavaScript, é comum a necessidade de convertê-las em uma string legível para o usuário. Isso pode ser útil em diversas situações, como ao exibir um calendário ou em formulários de reserva online. Felizmente, JavaScript possui uma função dedicada para essa tarefa, tornando o processo bastante simples.

## Como Fazer

A função `toString()` é utilizada para converter um objeto Date em uma string. Vejamos um exemplo:

```Javascript
let data = new Date();
let dataString = data.toString();

console.log(dataString);
```

A saída desse código seria algo como `"Sex Nov 19 2021 16:21:30 GMT-0300 (Horário Padrão de Brasília)"`. Note que a função `toString()` aceita opções adicionais, como idioma e formatação, mas os valores padrão geralmente são suficientes.

Outra maneira de converter uma data em uma string é utilizando a função `toLocaleString()`. Diferentemente da `toString()`, essa função permite especificar o idioma e formato da data. Vejamos um exemplo:

```Javascript
let data = new Date();
let opcoes = {weekday: "long", year: "numeric", month: "long", day: "numeric", hour: "numeric", minute: "numeric"};

let dataString = data.toLocaleString("pt-BR", opcoes);

console.log(dataString);
```

A saída seria algo como `"sexta-feira, 19 de novembro de 2021 às 16:21"`. É importante lembrar que as opções podem variar de acordo com o idioma selecionado.

## Mergulho Profundo

Além das funções mencionadas, JavaScript possui algumas outras opções para converter datas em strings, como `toDateString()` e `toISOString()`. Essas funções permitem mais controle sobre o formato e podem ser úteis em casos específicos. É recomendado ler a documentação oficial para explorar todas as possibilidades.

## Veja Também

- [Documentação oficial do objeto Date em JavaScript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Outros métodos de conversão de datas para strings em JavaScript](https://www.w3schools.com/jsref/jsref_obj_date.asp)
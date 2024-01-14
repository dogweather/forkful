---
title:    "TypeScript: Convertendo uma data em uma string"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por que?

Algumas vezes, em programação, precisamos exibir uma data em formato de texto. Isso pode ser necessário para fins de formatação, apresentação visual ou para armazenamento em um banco de dados. A conversão de uma data em uma string é uma tarefa comum em projetos TypeScript e entender como fazê-la pode ser muito útil.

## Como fazer

Aqui vamos explorar como converter uma data em uma string em TypeScript. Vamos supor que temos uma data no formato UTC e queremos exibi-la em um formato mais legível, como "dd/mm/aaaa". A sintaxe abaixo é o que usaremos para este exemplo:

```TypeScript
const data = new Date(); //cria um objeto data com a data atual
const dia = data.getDate(); //retorna o dia do mês
const mes = data.getMonth() + 1; //retorna o mês (0-11), por isso adicionamos 1
const ano = data.getFullYear(); //retorna o ano com 4 dígitos

//formata a data no formato desejado
const dataFormatada = `${dia}/${mes}/${ano}`;

console.log(dataFormatada); //saída: 27/08/2021
```

## Aprofundando-se

Existem várias funções úteis para formatar datas em TypeScript. Além do `getDate()`, `getMonth()` e `getFullYear()` utilizados no exemplo acima, temos também:

- `getDay()` - retorna o dia da semana (0-6).
- `getHours()` - retorna a hora do dia (0-23).
- `getMinutes()` - retorna os minutos (0-59).
- `getSeconds()` - retorna os segundos (0-59).
- `getMilliseconds()` - retorna os milissegundos (0-999).

Podemos utilizar essas funções em conjunto para criar uma string com as informações completas da data. Por exemplo:

```TypeScript
const data = new Date();
const dia = data.getDate();
const mes = data.getMonth() + 1;
const ano = data.getFullYear();
const hora = data.getHours();
const minutos = data.getMinutes();
const segundos = data.getSeconds();

//formata a data e a hora no formato desejado
const dataFormatada = `${dia}/${mes}/${ano} às ${hora}:${minutos}:${segundos}`;

console.log(dataFormatada); //saída: 27/08/2021 às 11:35:22
```

Para mais informações sobre todas as funções disponíveis para manipulação de datas em TypeScript, consulte a documentação oficial.

## Veja também

- Documentação oficial do TypeScript sobre manipulação de datas: https://www.typescriptlang.org/docs/handbook/datetime.html
- Tutorial em vídeo sobre como converter datas em TypeScript: https://www.youtube.com/watch?v=SjM
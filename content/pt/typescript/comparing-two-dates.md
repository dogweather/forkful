---
title:    "TypeScript: Comparando duas datas"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por que comparar duas datas em TypeScript?

Comparar datas é uma tarefa comum em programação e pode ser especialmente útil em projetos em TypeScript. Ao comparar duas datas, é possível determinar qual é a mais recente, calcular diferenças entre elas ou verificar se uma data está dentro de um determinado período. 

## Como fazer isso em TypeScript

Para comparar duas datas em TypeScript, podemos usar o objeto Date da linguagem. Esse objeto possui uma série de métodos que nos permitem realizar operações com datas, incluindo a comparação entre elas. Veja um exemplo de como podemos comparar duas datas em TypeScript:

```
// Criando duas datas
const data1 = new Date(2021, 5, 12);
const data2 = new Date(2021, 5, 10);

// Comparando as datas
if(data1 > data2) {
  console.log("data1 é mais recente que data2");
} else if(data1 < data2) {
  console.log("data2 é mais recente que data1");
} else {
  console.log("As datas são iguais");
}
```

O código acima irá imprimir "data1 é mais recente que data2" no console. Além do operador `>` (maior que) usado no exemplo, podemos também utilizar os operadores `>=` (maior ou igual a), `<` (menor que) e `<=` (menor ou igual a) para comparar as datas.

## Mergulho profundo

Uma das principais coisas a se ter em mente ao comparar datas em TypeScript é que, embora dois objetos Date possam parecer iguais quando impressos no console, eles podem ter milissegundos de diferença. Isso pode causar resultados surpreendentes ao compará-los diretamente usando os operadores. Para evitar isso, podemos usar o método `getTime()` do objeto Date, que retorna a quantidade de milissegundos desde 1 de janeiro de 1970 até a data especificada. Veja um exemplo:

```
// Criando duas datas com milissegundos diferentes
const data1 = new Date(2021, 5, 12, 10, 30, 0, 0);
const data2 = new Date(2021, 5, 12, 10, 30, 0, 500);

// Imprimindo as datas no console
console.log(data1); // Mon Jun 12 2021 10:30:00 GMT-0300 (Horário Padrão de Brasília)
console.log(data2); // Mon Jun 12 2021 10:30:00 GMT-0300 (Horário Padrão de Brasília)

// Comparando as datas
if(data1 > data2) {
  console.log("data1 é mais recente que data2");
} else if(data1 < data2) {
  console.log("data2 é mais recente que data1");
} else {
  console.log("As datas são iguais");
}

// Output: As datas são iguais
```

No exemplo acima, embora as datas sejam ligeiramente diferentes (uma com 0 milissegundos e outra com 500 milissegundos), a comparação direta usando os operadores resulta em "As datas são iguais". Porém, ao usar o método `getTime()`, obtemos um resultado correto.

## Veja também

- [Documentação oficial do TypeScript para o objeto Date](https://www.typescriptlang.org/docs/handbook/classes.html#date)
- [Guia de comparação de datas em TypeScript](https://dev.to/algobot76/comparing-dates-in-typescript-2f0p)
- [Tutorial em vídeo sobre como comparar datas em TypeScript](https://www.youtube.com/watch?v=fWvebF0RVZU&t=40s)
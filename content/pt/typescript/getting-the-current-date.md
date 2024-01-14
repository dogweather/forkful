---
title:    "TypeScript: Desobtenção da data atual"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por que?

Atualmente, a obtenção da data atual é uma tarefa comum em muitos projetos de programação. A data atual pode ser usada para diversas finalidades, como registrar a data de criação de um documento, verificar o tempo de vida de uma transação ou simplesmente exibir a data atual para o usuário. Independentemente do motivo, é importante saber como obter a data atual em um programa TypeScript.

## Como fazer

Para obter a data atual em TypeScript, podemos usar o objeto "Date". Ele possui um método chamado "getDate()" que retorna o dia atual do mês e outro método chamado "getMonth()" que retorna o mês atual. Em seguida, podemos combinar esses valores para obter a data completa. Veja o código abaixo:

```TypeScript
let data = new Date();
let dia = data.getDate();
let mes = data.getMonth() + 1; // os meses são indexados a partir do 0, por isso somamos 1
console.log(`A data atual é ${dia}/${mes}`);
```

O código acima irá imprimir no console a data atual no formato "dia/mês". Por exemplo, se hoje fosse o dia 25 de agosto, a saída seria "A data atual é 25/08".

Você também pode querer incluir o ano na data atual. Para isso, basta usar o método "getFullYear()" do objeto Date. Veja o exemplo abaixo:

```TypeScript
let data = new Date();
let dia = data.getDate();
let mes = data.getMonth() + 1; // os meses são indexados a partir do 0, por isso somamos 1
let ano = data.getFullYear();
console.log(`A data atual é ${dia}/${mes}/${ano}`);
```

Agora, a saída seria "A data atual é 25/08/2021".

## Profundidade

Além dos métodos mencionados acima, o objeto Date possui muitos outros para manipular e obter informações sobre datas. Alguns exemplos são "getDay()" para obter o dia da semana, "getHours()" para obter a hora atual e "toDateString()" para obter uma representação em formato de string da data atual.

Também é importante notar que o objeto Date é baseado no fuso horário do sistema onde o código está sendo executado. Se for necessário, é possível definir um fuso horário específico usando o método "setTimezoneOffset()". Além disso, é possível manipular datas e horas em diferentes formatos com bibliotecas externas, como a Moment.js.

## Veja também

- [Documentação oficial do objeto Date em TypeScript](https://www.typescriptlang.org/docs/handbook/datetime.html)
- [Guia de uso do Moment.js](https://momentjs.com/docs/)
- [Exemplo de manipulação de datas em TypeScript](https://dev.to/dkoshop/type-safe-javascript-date-manipulation-with-typescript-1kmd)
---
title:                "TypeScript: Obtendo a data atual"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que

A obtenção da data atual é um recurso essencial em muitas aplicações TypeScript. Saber a data atual pode ajudar a registrar eventos, agendar tarefas e fornecer informações relevantes para o usuário. Além disso, a obtenção da data atual também é importante para garantir que a aplicação está funcionando corretamente e para evitar erros devido a diferenças de fuso horário.

## Como fazer

Para obter a data atual em TypeScript, podemos usar o objeto Date. Este objeto possui vários métodos para retornar informações de data e hora, como ano, mês, dia, hora e minuto. Vamos ver alguns exemplos de como usar o objeto Date para obter a data atual em diferentes formatos:

```TypeScript
let dataAtual = new Date();

// Data no formato DD/MM/AAAA
console.log(dataAtual.getDate() + '/' + (dataAtual.getMonth() + 1) + '/' + dataAtual.getFullYear());

// Data no formato AAAA-MM-DD
console.log(dataAtual.toISOString().slice(0,10));
```

A saída destes exemplos seria, respectivamente: 12/07/2021 e 2021-07-12.

Também é possível obter informações mais detalhadas, como a hora e o fuso horário. Vamos ver um exemplo que usa o método getHours() e getTimezoneOffset():

```TypeScript
let dataAtual = new Date();
console.log(dataAtual.getHours() + ' horas');
console.log('Fuso horário: ' + dataAtual.getTimezoneOffset()/60 + ' horas atrás');
```

A saída deste exemplo seria, por exemplo: 15 horas e Fuso horário: 3 horas atrás.

## Aprofundando

Agora que sabemos como obter a data atual em diferentes formatos, é importante entender como o objeto Date funciona por dentro. O valor retornado pelo método getDate() por exemplo, corresponde ao dia do mês em que estamos. No entanto, o mês começa com o valor 0, então precisamos adicionar 1 ao mês retornado pelo método getMonth(). Além disso, é importante ter em mente que o objeto Date é baseado no fuso horário do sistema e pode variar de acordo com as configurações de localização.

Para mais informações sobre o objeto Date e seus métodos, consulte a documentação da linguagem TypeScript.

## Veja também

- [Documentação oficial do objeto Date em TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-2.html#the-date-type)
- [Como formatar datas em TypeScript](https://www.geeksforgeeks.org/how-to-format-dates-in-typescript/)
- [Diferenças entre Date e Moment em TypeScript](https://www.typescriptlang.org/docs/handbook/intro-to-js-ts.html#working-with-dates-strings-enums)
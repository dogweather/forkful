---
title:    "Javascript: Obtendo a data atual"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Por que obter a data atual em JavaScript?

Quando escrevemos código, muitas vezes precisamos de informações da data atual para realizar alguma ação específica. Pode ser para registrar quando um usuário se cadastrou em um site, calcular a idade de uma pessoa ou simplesmente exibir a data no formato correto. Em JavaScript, obter a data atual é uma tarefa simples e pode ser muito útil em várias situações. Neste artigo, vamos explorar como obter a data atual em JavaScript e como utilizá-la em nosso código.

## Como Fazer

Para obter a data atual em JavaScript, podemos utilizar o objeto `Date()`. Sempre que criamos uma instância desse objeto, ele nos fornece a data e a hora atual. Vamos dar uma olhada em alguns exemplos:

```
// Obtendo a data atual
let dataAtual = new Date();
console.log(dataAtual);

// Saída: Mon Jul 20 2020 12:53:08 GMT-0300 (Horário Padrão de Brasília)
```

Podemos ver que a data e hora foram impressas no console no formato padrão. Mas podemos personalizar como queremos que a data seja exibida usando os métodos do objeto `Date()`.

```
// Obtendo apenas o ano atual
let anoAtual = new Date().getFullYear();
console.log(anoAtual);

// Saída: 2020

// Obtendo o dia atual
let diaAtual = new Date().getDay();
console.log(diaAtual);

// Saída: 1 (segunda-feira)
```

Também podemos criar data e hora específicas passando os parâmetros para o objeto `Date()`. Por exemplo:

```
// Criando uma data específica
let dataNascimento = new Date(1995, 02, 15);

console.log(dataNascimento);

// Saída: Wed Mar 15 1995 00:00:00 GMT-0300 (Horário Padrão de Brasília)
```

Existem muitos métodos que podemos utilizar para obter informações específicas da data e hora, como `getMonth()`, `getHours()`, `getMinutes()`, etc. Também podemos adicionar ou subtrair valores de uma data, realizar operações matemáticas com datas e muito mais. O importante é entendermos que a data atual pode ser obtida facilmente em JavaScript e podemos manipulá-la de acordo com nossas necessidades.

## Aprofundando-se

O objeto `Date()` em JavaScript pode ser um pouco confuso, pois algumas funções como `getMonth()` e `getDay()` retornam valores numéricos que não correspondem exatamente aos meses e dias do calendário. Isso ocorre porque esses métodos retornam os valores começando em 0, ou seja, janeiro é representado por 0 e dezembro por 11. Por isso, é sempre importante ler a documentação ou fazer uma pesquisa para entender os valores retornados por cada método.

Outro ponto importante é que o objeto `Date()` é baseado no horário do sistema do computador em que o código está sendo executado. Isso significa que se o usuário estiver em um fuso horário diferente, a data e hora exibidos serão diferentes. Para resolver esse problema, podemos usar as funções `getUTCDate()` e `getUTCHours()` para obter a data e hora no horário universal (UTC).

É importante também lembrar que o objeto `Date()` é mutável e pode ser modificado acidentalmente. Portanto, é uma boa prática criar uma cópia da data atual se você precisar utilizá-la em diferentes partes do código.

## Veja também

- [Documentação oficial do objeto Date() em JavaScript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Manipulando datas em JavaScript](https://www.w3schools.com/js/js_dates.asp)
- [Métodos avançados do objeto Date() em JavaScript](https://www.toptal.com/software/definitive-guide-to-datetime-manipulation)
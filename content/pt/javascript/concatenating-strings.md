---
title:                "Javascript: Concatenando strings"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

Ao escrever código em Javascript, é comum precisarmos combinar diferentes partes de texto para formar uma string. Isso pode ser necessário para exibir informações dinamicamente no navegador, enviar mensagens personalizadas para usuários ou até mesmo para organizar dados em um formato específico. Uma das formas mais comuns de fazer isso é usando a concatenação de strings.

## Como fazer

A concatenação de strings em Javascript é simples e flexível. Podemos usar o operador `+` para adicionar duas ou mais strings juntas, ou o método `concat()` para unir strings em um único array. Vejamos alguns exemplos:

```
let nome = "João";
let sobrenome = "Silva";

let nomeCompleto = nome + " " + sobrenome;
console.log(nomeCompleto); // Saída: "João Silva"

let frutas = ["maçã", "banana", "laranja"];
let listaDeCompras = "Minha lista de compras: " + frutas.concat("morango");
console.log(listaDeCompras); // Saída: "Minha lista de compras: maçã, banana, laranja, morango"
```

Podemos ver que a concatenação nos permite combinar strings de diferentes variáveis ou arrays de forma rápida e eficiente. Além disso, também podemos usar o método `toString()` para transformar outros tipos de dados em strings e depois concatená-los.

## Mergulho profundo

A concatenação de strings pode ser poderosa quando combinada com outras técnicas, como interpolação de strings, que nos permite inserir valores de variáveis diretamente em uma string. Podemos usar a sintaxe `${variavel}` para isso, como demonstrado no exemplo abaixo:

```
let nome = "Maria";
let sobrenome = "Fernandes";
let idade = 35;

let apresentacao = `Olá, meu nome é ${nome} ${sobrenome} e tenho ${idade} anos.`;
console.log(apresentacao); // Saída: "Olá, meu nome é Maria Fernandes e tenho 35 anos."
```

Também é importante lembrar que a concatenação de strings pode afetar o desempenho do código, especialmente quando feita em iterações repetidas. Nesses casos, é recomendado o uso de métodos específicos para manipulação de strings, como `join()` ou `substr()`, que podem ser mais eficientes.

## Veja também

Aqui estão alguns links úteis para saber mais sobre concatenação e manipulação de strings em Javascript:

- [Documentação oficial do método concat()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [Tutorial sobre interpolação de strings em Javascript](https://www.digitalocean.com/community/tutorials/how-to-do-string-interpolation-in-javascript-pt)
- [Dicas para melhorar o desempenho em manipulação de strings](https://dev.to/franmarxo/11-javascript-performance-boosting-gunpowder-55jk)
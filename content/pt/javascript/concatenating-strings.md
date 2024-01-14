---
title:                "Javascript: Unindo strings"
simple_title:         "Unindo strings"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

A concatenação de strings é uma técnica muito comum em programação JavaScript, especialmente em tarefas envolvendo manipulação de texto. Essa técnica permite combinar várias strings em uma única string, facilitando o trabalho com dados e tornando o código mais eficiente. 

## Como fazer

A concatenação de strings é feita utilizando o operador "+". Veja um exemplo de código abaixo: 

```Javascript
let nome = "João";
let sobrenome = "Silva";

let nomeCompleto = nome + " " + sobrenome;
console.log(nomeCompleto);
```

Nesse exemplo, declaramos duas variáveis com nomes e sobrenomes, e em seguida utilizamos o operador "+" para concatenar as duas strings em uma única variável chamada "nomeCompleto". Ao executar o código, a saída será "João Silva". 

Também é possível utilizar o método `concat()` para concatenar duas ou mais strings. Veja um exemplo: 

```Javascript
let parte1 = "Esse é";
let parte2 = "um exemplo";
let parte3 = "de concatenação.";

let fraseCompleta = parte1.concat(" ", parte2, " ", parte3);
console.log(fraseCompleta);
```

Nesse caso, utilizamos o método `concat()` para unir as três partes de uma frase em uma única string. A saída será "Esse é um exemplo de concatenação." 

É importante lembrar que ao trabalhar com números e strings na concatenação, os números serão convertidos automaticamente em strings. Por exemplo: 

```Javascript
let numero = 42;
let texto = " é a resposta para a vida, o universo e tudo mais.";

let resposta = numero + texto;
console.log(resposta);
```

A saída será "42 é a resposta para a vida, o universo e tudo mais." 

## Mergulho profundo

Além do operador "+" e do método `concat()`, também podemos utilizar a template string para fazer a concatenação de strings de forma mais prática e legível. Com ela, podemos incluir variáveis e expressões dentro de uma string utilizando a sintaxe `${}`. Veja um exemplo: 

```Javascript
let nome = "Maria";
let sobrenome = "Souza";

console.log(`Meu nome é ${nome} ${sobrenome}.`);
```

A saída será "Meu nome é Maria Souza.". 

Também é possível utilizar o método `join()` para concatenar elementos de um array em uma única string, utilizando um caractere ou string como separador. Por exemplo: 

```Javascript
let frutas = ["maçã", "banana", "laranja"];
console.log(`Eu gosto de ${frutas.join(", ")}s.`);

// Saída: Eu gosto de maçãs, bananas, laranjas.
```

## Veja também

- [Documentação do JavaScript sobre concatenação de strings](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Grammar_and_types#Concatenacao_de_strings)
- [Explicação detalhada sobre template strings](https://www.youtube.com/watch?v=QyLiGElrSI4) (em português)
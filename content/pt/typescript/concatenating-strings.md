---
title:                "TypeScript: Concatenando strings"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que usar a concatenação de strings
A concatenação de strings é uma técnica comum e útil na programação TypeScript. É utilizada para combinar duas ou mais strings em uma só, permitindo que você crie mensagens dinâmicas e personalize a saída do seu código. 

## Como fazer
Para concatenar strings em TypeScript, você pode utilizar o operador `+` ou o método `concat()`. Veja abaixo alguns exemplos de como utilizar essas duas formas:

```TypeScript 
// Utilizando o operador +
let nome = "Maria";
let sobrenome = "Silva";
let nomeCompleto = nome + " " + sobrenome;
console.log(nomeCompleto); // Output: Maria Silva

// Utilizando o método concat()
let texto1 = "Olá";
let texto2 = "mundo";
let textoCompleto = texto1.concat(" ", texto2);
console.log(textoCompleto); // Output: Olá mundo 
```

Como mostrado nos exemplos acima, a concatenação de strings permite que você adicione espaço entre as palavras ou caracteres especiais, como emojis ou símbolos, entre as strings.

## Mergulho profundo
Além dos métodos mencionados, existem outras formas de concatenar strings em TypeScript. Você pode utilizar a template string, indicada pelo sinal de crase (`), que permite que você insira variáveis dentro da string utilizando a sintaxe `${ variavel }`. Veja um exemplo desse método abaixo:

```TypeScript
// Utilizando template string
let idade = 30;
let mensagem = `Eu tenho ${idade} anos.`;
console.log(mensagem); // Output: Eu tenho 30 anos.
```

Também é importante lembrar que é possível concatenar mais de duas strings ao mesmo tempo, basta adicionar cada uma delas na ordem que deseja que elas apareçam. Além disso, é possível utilizar a concatenação de strings com outros tipos de dados, como números ou booleanos. 

A concatenação de strings pode ser muito útil em diferentes situações, como na criação de mensagens personalizadas e na manipulação de dados em geral. É importante entender os diferentes métodos disponíveis e escolher o mais adequado para cada caso.

## Veja também
Se você quer se aprofundar ainda mais no assunto, confira os links abaixo para mais informações sobre a concatenação de strings em TypeScript:

- [Documentação oficial do TypeScript sobre strings](https://www.typescriptlang.org/docs/handbook/strings.html)
- [Tutorial sobre concatenação de strings em TypeScript](https://www.javatpoint.com/typescript-string-concatenation)
- [Vídeo explicando como utilizar a template string em TypeScript](https://www.youtube.com/watch?v=VuS_mGrlvt8)
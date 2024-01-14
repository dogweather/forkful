---
title:                "Javascript: Transformando uma sequência em letras maiúsculas"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string em Javascript

Se você já trabalhou com Javascript, provavelmente se deparou com o desafio de capitalizar uma string. Isso significa transformar a primeira letra de cada palavra em maiúscula. Mas por que alguém gostaria de fazer isso? Existem diversas situações em que isso pode ser útil, como por exemplo, ao exibir nomes ou títulos de forma estilizada em um site ou aplicativo.

## Como capitalizar uma string em Javascript

Existem diferentes formas de capitalizar uma string em Javascript. Aqui, vamos abordar duas delas: utilizando um loop e utilizando um método nativo na linguagem.

#### Utilizando um loop

Usando um loop for, podemos percorrer cada caractere da string e, caso seja uma letra minúscula, transformá-la em maiúscula.

```Javascript
let string = "apenas um exemplo";

for(let i = 0; i < string.length; i++){
  // verifica se o caractere atual é uma letra minúscula
  if(string[i] === string[i].toLowerCase()){
    // transforma o caractere em maiúscula e o concatena à nova string
    string = string.substring(0, i) + string[i].toUpperCase() + string.substring(i + 1);
  }
}
console.log(string); // "Apenas Um Exemplo"
```

#### Utilizando o método toLocaleUpperCase()

O Javascript possui um método nativo chamado toLocaleUpperCase() que transforma uma string em maiúscula de acordo com as configurações do idioma do usuário.

```Javascript
let string = "uma outra forma";

string = string.toLowerCase().split(" ").map(word => word.charAt(0).toLocaleUpperCase() + word.substring(1)).join(" ");
console.log(string); // "Uma Outra Forma"
```

## Aprofundando-se na capitalização de strings

Além das formas apresentadas acima, existem ainda outras maneiras de capitalizar uma string em Javascript. É possível utilizar expressões regulares ou bibliotecas externas que facilitam esse processo. Além disso, é importante ter em mente que a capitalização de strings pode variar dependendo do idioma utilizado, pois em alguns casos, palavras inteiras devem ser escritas em maiúsculo. Por isso, é importante estar atento ao contexto e às necessidades específicas de cada projeto.

## Veja também

- Documentação oficial do método toLocaleUpperCase(): https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleUpperCase
- Como utilizar expressões regulares em Javascript: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Regular_Expressions
- Biblioteca Lodash para manipulação de strings em Javascript: https://lodash.com/docs/4.17.15#camelCase
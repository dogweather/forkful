---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "TypeScript: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que 

Às vezes, em nossos códigos, nos deparamos com a necessidade de remover caracteres que correspondem a um padrão específico. Isso pode ser útil para limpar strings, validar entradas ou em outras situações em que precisamos manipular dados de certa forma. Felizmente, com o TypeScript, podemos fazer isso de maneira simples e eficiente.

## Como fazer

Para deletar caracteres que correspondem a um padrão em TypeScript, podemos usar o método `replace()` da classe `String`. Este método aceita uma expressão regular como primeiro parâmetro e uma string vazia como segundo parâmetro, para indicar que queremos substituir o padrão por nada. 
Aqui está um exemplo prático de como usá-lo:

```TypeScript
const frase = "Apenas um exemplo 123!";
const novaFrase = frase.replace(/[0-9]/g, "");
console.log(novaFrase); // Saída: "Apenas um exemplo !"
```

Neste exemplo, estamos usando uma expressão regular que corresponde a qualquer dígito de 0 a 9 e, em seguida, utilizando o método `replace()` para substituir todos esses dígitos por uma string vazia, ou seja, os excluindo da frase original. 
Também podemos usar expressões regulares mais complexas, dependendo do padrão que queremos deletar. Podemos combinar diversas regras e até mesmo adicionar modificadores, como o `i` para tornar a expressão insensível a maiúsculas e minúsculas. 
Vale lembrar que, ao usar o método `replace()`, a frase original não é modificada. O método retorna uma nova string com as alterações.

## Mergulho Profundo

Para entender melhor como o método `replace()` funciona, podemos dar uma olhada mais aprofundada na estrutura de uma expressão regular. 
Em resumo, uma expressão regular é uma sequência de caracteres que define um padrão específico no qual queremos encontrar correspondências. Isso pode incluir caracteres literais, classes de caracteres, quantificadores, entre outros elementos. 
Ao usar o `replace()` com uma expressão regular, podemos dizer que queremos substituir todas as correspondências do padrão encontrado na frase original. 
Por exemplo, se quisermos substituir todas as letras maiúsculas por asteriscos, podemos fazer o seguinte:

```TypeScript
const frase = "Apenas um Exemplo 123!";
const novaFrase = frase.replace(/[A-Z]/g, "*");
console.log(novaFrase); // Saída: "*penas um xemplo 123!"
```

Neste exemplo, estamos usando uma classe de caracteres que corresponde a todas as letras maiúsculas do alfabeto e substituindo-as por um asterisco. O modificador `g` indica que queremos substituir todas as correspondências, e não apenas a primeira.
Com isso, podemos perceber como o uso da expressão regular pode ser poderoso e versátil em nossos códigos em TypeScript.

## Veja também

- Documentação oficial do método `replace()`: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- Tutorial sobre expressões regulares em TypeScript: https://www.typescriptlang.org/docs/handbook/regexp.html
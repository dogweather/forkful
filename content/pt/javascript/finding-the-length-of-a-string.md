---
title:                "Encontrando o comprimento de uma string"
html_title:           "C: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Aprendendo JavaScript: Descobrindo o Comprimento de uma String

## O Que É e Por Quê?

Descobrir o comprimento de uma String significa determinar o número de caracteres que ela contém. Programadores fazem isso para manipular, validar ou comparar Strings com eficácia em seus códigos.

## Como Fazer:

Aqui está um exemplo de como obter o comprimento de uma String em JavaScript. 

```JavaScript
let texto = "Olá, mundo!";
console.log(texto.length);  // Saída: 12
```
A propriedade `.length` retorna o número de caracteres na string. No exemplo, a saída será 12, que inclui caracteres e espaços.

## Aprofundamento

Embora a propriedade `.length` seja a maneira mais comum e rápida de obter o comprimento de uma string em JavaScript, existem alternativas dependendo do problema que você está resolvendo.

### Contexto Histórico

Antes da existência da propriedade `.length`, os programadores tinham que iterar pelos caracteres de uma string para contar seu comprimento.

```Javascript
function comprimentoDaString(str) {
    let i = 0;
    while (str[i] !== undefined) {
        i++;
    }
    return i;
}
console.log(comprimentoDaString("Olá, mundo!")); // Saída: 12
```

### Alternativas

Em algumas situações, você pode querer contar apenas os caracteres que não sejam espaços. Em tal caso, você pode usar o método `.replace()` para remover espaços e, em seguida, calcular o comprimento.

```Javascript
let texto = "Olá, mundo!";
let textoSemEspacos = texto.replace(/ /g, '');
console.log(textoSemEspacos.length);  // Saída: 11
```

## Veja Também

1. [MDN Web Docs: String.length](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/length)
2. [JavaScript.Info: Strings](https://pt.javascript.info/string)
3. [W3Schools: JavaScript String Length Property](https://www.w3schools.com/jsref/jsref_length_string.asp)

Espero que este guia simplificado tenha te ajudado a entender melhor como encontrar o comprimento de uma string em JavaScript. Continue praticando e explore as referências fornecidas para dominar os conceitos.
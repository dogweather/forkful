---
date: 2024-01-20 17:47:57.836199-07:00
description: "Descobrir o tamanho de uma string \xE9 basicamente contar quantos caracteres\
  \ ela tem. Programadores fazem isso para validar entradas, manipular textos ou\u2026"
lastmod: '2024-03-11T00:14:20.688499-06:00'
model: gpt-4-1106-preview
summary: "Descobrir o tamanho de uma string \xE9 basicamente contar quantos caracteres\
  \ ela tem. Programadores fazem isso para validar entradas, manipular textos ou\u2026"
title: Descobrindo o comprimento de uma string
---

{{< edit_this_page >}}

## What & Why?
Descobrir o tamanho de uma string é basicamente contar quantos caracteres ela tem. Programadores fazem isso para validar entradas, manipular textos ou simplesmente para saber a quantidade de dados a serem processados.

## How to:
Usar `length` em JavaScript é moleza. Olha só:

```javascript
let saudacao = "Olá, mundo!";
let tamanho = saudacao.length;
console.log(tamanho); // Saída: 12
```

Se mudar o texto, o `length` muda junto. Fácil, né?

```javascript
let frase = "Programar é bacana!";
console.log(frase.length); // Saída: 19
```

## Deep Dive
O `.length` já é velho conhecido em JavaScript. Foi introduzido na primeira versão e desde então é o jeito padrão de medir strings. Existem outras formas de contar caracteres, como looping através dos caracteres ou usando expressões regulares, mas `.length` é o jeito mais direto e eficiente.

### Um pouco de história
No começo, tudo na web era mais simples. Quando JavaScript foi criado, tudo era feito para ser rápido e fácil. Com o tempo, as coisas foram complicando, mas `.length` se manteve igual.

### Alternativas
Podemos, por exemplo, usar `split` para dividir uma string em um array e depois contar os elementos:

```javascript
let mensagem = "JavaScript é legal!";
let tamanhoArray = mensagem.split('').length;
console.log(tamanhoArray); // Saída: 20
```

Mas isso é mais lento e, na real, desnecessário.

### Implementação
Quando você acessa o `.length` de uma string, o JavaScript já sabe a resposta. Isso porque o tamanho da string é armazenado na memória quando a string é criada ou modificada, então não requer cálculo quando você pede essa informação.

## See Also
- MDN (Mozilla Developer Network) sobre strings e `.length`: [MDN String.length](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- Tutorial sobre strings em JavaScript: [JavaScript String Methods](https://www.w3schools.com/js/js_string_methods.asp)
- Discussão sobre a performance de `.length`: [Stack Overflow: Does .length cause a re-count every time?](https://stackoverflow.com/questions/45959125/does-length-cause-a-re-count-every-time)

---
title:                "Convertendo uma string para minúsculas"
html_title:           "TypeScript: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que

Muitas vezes é necessário converter uma string para letra minúscula ao lidar com dados de entrada ou fazer comparações de texto em uma aplicação TypeScript. Isso garante que todos os caracteres sejam tratados da mesma forma, independentemente de sua capitalização.

## Como fazer

```TypeScript
const texto = "Olá, MUndo!";
const textoMin = texto.toLowerCase();
console.log(textoMin); // saída: "olá, mundo!"
```

Outra forma de fazer a conversão é usando a função `toLowerCase()` diretamente em uma variável ou objeto que contenha a string:

```TypeScript
let nome = "João";
nome = nome.toLowerCase();
console.log(nome); // saída: "joão"
```

## Deep Dive

Ao converter uma string para letra minúscula, é importante lembrar que apenas as letras serão alteradas, os caracteres especiais como acentos e símbolos permanecerão os mesmos. Isso pode ser um problema ao realizar comparações de texto, pois é necessário tratar esses caracteres antes de fazer a comparação.

Por exemplo, se tivermos a seguinte comparação:

```TypeScript
const texto1 = "café";
const texto2 = "Café";
if (texto1 == texto2) {
    console.log("As strings são iguais!");
} else {
    console.log("As strings são diferentes!");
}
```

A saída seria "As strings são diferentes!", pois a letra "C" maiúscula é considerada diferente da letra "c" minúscula.

Para evitar essa situação, podemos usar a função `localeCompare()` para comparar as strings independentemente da capitalização:

```TypeScript
const texto1 = "café";
const texto2 = "Café";
if (texto1.localeCompare(texto2) === 0) {
    console.log("As strings são iguais!");
} else {
    console.log("As strings são diferentes!");
}
```

A saída agora seria "As strings são iguais!", pois a função `localeCompare()` realiza a comparação levando em conta a ordem alfabética, independentemente da capitalização.

## Veja também

- [Documentação oficial do TypeScript para a função `toLowerCase()`](https://www.typescriptlang.org/docs/handbook/strings.html#lowercase-and-uppercase-strings)
- [Função `localeCompare()` no JavaScript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/localeCompare)
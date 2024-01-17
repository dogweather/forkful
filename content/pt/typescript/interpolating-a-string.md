---
title:                "Interpolando uma string"
html_title:           "TypeScript: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O que é e por que fazemos isso?
Interpolar uma string pode parecer uma tarefa complicada, mas é simplesmente a ação de inserir dinamicamente uma variável em uma string. Isso é útil para criar strings mais dinâmicas e personalizadas em nosso código.

## Como fazer:
Aqui está um exemplo simples de como interpolar uma string em TypeScript:

```TypeScript
let name = "Maria";
let greeting = `Olá ${name}, bem-vinda!`;
console.log(greeting);
```

Isso irá imprimir "Olá Maria, bem-vinda!" no console. Como você pode ver, usamos o símbolo de cifrão e chaves dentro da string delimitada por acentos graves para inserir a variável `name` dentro da string. É importante notar que esse recurso só funciona com strings delimitadas por acentos graves.

Podemos também utilizar a interpolação de strings em outras situações, como por exemplo, em uma função:

```TypeScript
function calculatePrice(quantity: number, price: number) {
  return `O preço total é ${quantity * price} reais.`;
}

console.log(calculatePrice(5, 10));
```

Isso irá imprimir "O preço total é 50 reais." no console.

## Detalhes mais profundos:
A interpolação de strings não é uma novidade, ela já existe em outras linguagens de programação, como por exemplo em Ruby. Além disso, é uma alternativa mais eficiente e legível do que a concatenação de strings.

Ao usar a interpolação de strings, o TypeScript irá compilar o código para utilizar o método `String.raw`, que é responsável por tratar as variáveis representadas pelas chaves dentro da string acentuada.

## Veja também:
- [Documentação do TypeScript sobre String Interpolation](https://www.typescriptlang.org/docs/handbook/basic-types.html#template-strings)
- [Artigo sobre String Interpolation no Medium](https://medium.com/@julianobattisti/javascript-o-que-%C3%A9-a-interpola%C3%A7%C3%A3o-de-strings-ca2417a55f3f)
- [Tutorial de TypeScript no YouTube (português)](https://www.youtube.com/watch?v=iAJPksAAS3Y)
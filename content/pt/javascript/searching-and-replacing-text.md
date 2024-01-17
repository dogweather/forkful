---
title:                "Busca e substituição de texto"
html_title:           "Javascript: Busca e substituição de texto"
simple_title:         "Busca e substituição de texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O que & porquê?
Substituir e pesquisar por texto é uma tarefa comum em programação. Isso envolve encontrar um determinado trecho de texto em um código e substituí-lo por outro. Os programadores realizam essa tarefa para corrigir erros, atualizar informações ou refatorar seu código.

## Como fazer:
Para substituir e pesquisar por texto em Javascript, podemos usar o método replace() da classe String. Este método aceita dois parâmetros: o texto a ser substituído e o novo texto que será usado para substituir o texto antigo.

Exemplo:
```
let texto = "Olá mundo!";
texto = texto.replace("mundo", "amigos");
console.log(texto);
```

Saída:
```
Olá amigos!
```

## Aprofundamento:
A substituição e pesquisa de texto é uma técnica amplamente utilizada desde os primeiros dias da programação. Inicialmente, os programadores precisavam escrever seus próprios algoritmos para realizar essa tarefa. Com o avanço das linguagens de programação, como o Javascript, surgiram métodos e funções que facilitam essa tarefa.

Uma alternativa ao método replace() é o método split() da classe String, que divide uma string em um array de substrings com base em um separador especificado. Combinado com o método join(), também da classe String, podemos criar um código semelhante ao método replace().

Exemplo:
```
let texto = "Olá mundo!";
texto = texto.split("mundo").join("amigos");
console.log(texto);
```

Saída:
```
Olá amigos!
```

## Veja também:
- [Método replace() no Mozilla Developer Network](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Método split() no Mozilla Developer Network](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/split)
- [Método join() no Mozilla Developer Network](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Array/join)
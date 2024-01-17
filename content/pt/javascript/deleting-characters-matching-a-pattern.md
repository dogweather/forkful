---
title:                "Excluindo caracteres correspondentes a um padrão"
html_title:           "Javascript: Excluindo caracteres correspondentes a um padrão"
simple_title:         "Excluindo caracteres correspondentes a um padrão"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Deletar caracteres que correspondem a um padrão é um processo comum na programação em Javascript. Isso permite aos programadores manipular cadeias de texto conforme necessário, removendo caracteres específicos que não são desejados ou necessários.

## Como fazer:
Existem várias maneiras de deletar caracteres que correspondem a um padrão em Javascript. Aqui estão dois exemplos usando expressões regulares:

```Javascript
// Exemplo 1: Usando o método replace()
let string = "Hello World!";
let novaString = string.replace(/[aeiou]/g, ''); // Output: Hll Wrld!

// Exemplo 2: Usando o método split() e join()
let string = "Hello World!";
let novaString = string.split(/[aeiou]/).join(''); // Output: Hll Wrld!
```

## Deep Dive:
Historicamente, expressões regulares são usadas para manipular texto em várias linguagens de programação. Elas são uma sequência específica de caracteres que formam um padrão de busca. Além disso, existem outras opções além do método 'replace()' em Javascript, como 'split()' e 'match()'. A escolha da abordagem depende das necessidades específicas do projeto em questão.

## Veja também:
- [Documentação do método replace() em Javascript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Documentação do método split() em Javascript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/split)
- [Documentação do método match() em Javascript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/match)
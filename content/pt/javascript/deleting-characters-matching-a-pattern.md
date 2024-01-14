---
title:    "Javascript: Excluindo caracteres que correspondem a um padrão"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que
Às vezes, em programação, pode ser necessário remover caracteres que correspondam a um determinado padrão em uma string. Isso pode ser útil para limpeza de dados ou para transformar uma string em outro formato específico. Neste post, vamos explorar como podemos fazer isso em Javascript.

## Como fazer
Para remover caracteres que correspondam a um padrão em uma string, podemos usar a função `replace()` do Javascript. Essa função aceita dois parâmetros: o padrão que desejamos encontrar e o que queremos substituir por esse padrão. Vamos dar uma olhada em um exemplo:

```Javascript
let meuTexto = "Olá, meu nome é João!";
meuTexto = meuTexto.replace(/[aeiou]/g, '');
console.log(meuTexto);

// Output: "Ol, m nm é J!"
```

Neste exemplo, usamos a expressão regular `/[aeiou]/g` como o padrão a ser encontrado e substituído. Basicamente, isso significa que estamos procurando por qualquer vogal na string e as substituindo por uma string vazia, fazendo com que essas vogais sejam removidas.

Outra maneira de remover caracteres que correspondam a um padrão é usando a mesma função `replace()`, mas desta vez fornecendo uma função como segundo parâmetro. Vamos ver outro exemplo:

```Javascript
let meuTexto = "Hoje é um lindo dia para aprender Javascript!";
meuTexto = meuTexto.replace(/[A-Z]/g, function(match) {
  return match.toLowerCase();
});
console.log(meuTexto);

// Output: "hoje é um lindo dia para aprender javascript!"
```

Neste caso, estamos usando a expressão regular `/[A-Z]/g` para encontrar todas as letras maiúsculas na string e substituí-las por sua versão minúscula usando uma função de callback.

## Profundidade
A função `replace()` do Javascript pode não ser apenas usada para substituir caracteres, mas também para executar outras ações, como por exemplo, acesso a grupos de captura em expressões regulares. Ao usar grupos de captura, podemos transformar nossa string de várias maneiras. Por exemplo:

```Javascript
let telefone = "Meu número é (55) 12345-6789";
let novoTelefone = telefone.replace(/\((\d{2})\) (\d{5})-(\d{4})/, '$2 $3');
console.log(novoTelefone);

// Output: "12345 6789"
```

Neste exemplo, estamos usando grupos de captura para extrair apenas os dígitos correspondentes ao código de área e ao número de telefone, que são os grupos 2 e 3, respectivamente. Então, na função de substituição, estamos retornando apenas esses grupos, o que resulta em um novo número de telefone formatado de maneira diferente.

## Veja também
- [Documentação oficial do Javascript: String.prototype.replace()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Expressões regulares em Javascript](https://www.regular-expressions.info/javascript.html)
- [Guia completo de Expressões Regulares em Javascript](https://javascript.info/regular-expressions)
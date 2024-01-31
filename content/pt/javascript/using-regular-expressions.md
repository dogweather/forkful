---
title:                "Utilizando expressões regulares"
date:                  2024-01-19
html_title:           "Bash: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"

category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O Que São & Por Que Usar?

Expressões regulares são padrões usados para encontrar combinações de caracteres em strings. Programadores usam-nas para validação de dados, busca e substituição de texto e para tarefas de análise de texto mais complexas, devido à sua poderosa flexibilidade.

## Como Fazer:

```javascript
// Validando um email
const validarEmail = (email) => /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email);
console.log(validarEmail("exemplo@dominio.com")); // true
console.log(validarEmail("exemplo@dominio")); // false

// Encontrando todos os números em uma string
const numeros = "Os números 2, 42 e 789 estão aqui.";
const regexNumeros = /\d+/g;
const resultado = numeros.match(regexNumeros);
console.log(resultado); // ["2", "42", "789"]
```

## Mergulho Profundo:

Historicamente, as expressões regulares têm suas raízes na teoria dos autômatos e na linguística formal. Linguagens de programação modernas, como JavaScript, implementam expressões regulares compatíveis com a sintaxe introduzida pelo Perl, que popularizou seu uso.

Alternativas às expressões regulares incluem o uso de métodos de string como `indexOf`, `split` e `replace`, mas estes são geralmente mais limitados e menos potentes para padrões complexos.

Quanto à implementação, JavaScript usa expressões regulares baseadas na biblioteca PCRE (Perl Compatible Regular Expressions). Uma expressão regular em JavaScript é um objeto `RegExp` que pode ser construído literalmente ou através de um construtor.

## Veja Também:

- MDN Web Docs sobre expressões regulares em JavaScript: [MDN RegExp](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Regular_Expressions)
- Testador de expressões regulares online: [RegExr](https://regexr.com/)
- Resumo de métodos e propriedades de `RegExp` no JavaScript: [JavaScript RegExp Reference](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)

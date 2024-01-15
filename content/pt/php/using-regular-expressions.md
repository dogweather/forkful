---
title:                "Utilizando expressões regulares"
html_title:           "PHP: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares em programação?

Expressões regulares são uma ferramenta poderosa e versátil que pode ser usada em várias linguagens de programação, incluindo PHP. Elas permitem que você encontre e manipule padrões de texto de forma eficiente, economizando tempo e tornando seu código mais conciso. Se você lida com manipulação de strings em seu trabalho de programação, aprender a utilizar expressões regulares pode ser extremamente benéfico.

## Como usar expressões regulares em PHP

Para usar expressões regulares em PHP, você precisa utilizar as funções `preg_match()` ou `preg_replace()`. Ambas recebem dois argumentos: o padrão de expressão regular que você deseja procurar e a string de texto em que deseja aplicá-lo. Por exemplo:

```PHP
$texto = "Olá, meu nome é João!";
if(preg_match("/João/", $texto)){
  echo "Olá João!";
} else {
  echo "Desculpe, não encontrei seu nome no texto.";
}
```

Este código utiliza a função `preg_match()` para procurar a string "João" no texto e, se encontrada, exibe uma mensagem de saudação. Se você estiver procurando por um padrão mais complexo, pode usar metacaracteres como `*` para indicar que uma determinada parte do padrão pode ser repetida várias vezes.

## Aprofundando-se em expressões regulares

Expressões regulares podem ser um pouco intimidadoras no início, mas com um pouco de prática e entendimento, elas podem ser incrivelmente úteis. É importante lembrar que elas seguem uma sintaxe específica e que pequenos detalhes podem alterar completamente o resultado.

Uma das melhores maneiras de aprender a utilizar expressões regulares é praticando. Você pode encontrar vários recursos online que oferecem testes e tutoriais interativos. Além disso, é sempre útil ter um guia de referência à mão para consultar os metacaracteres e suas funções.

## Veja também
- [Site do PHP - Expressões Regulares](https://www.php.net/manual/pt_BR/intro.pcre.php)
- [Tutorial Expressões Regulares da MDN](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Regex101 - Verificador de Expressões Regulares](https://regex101.com/)
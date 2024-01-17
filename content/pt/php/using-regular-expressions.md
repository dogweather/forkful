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

## O que é e porquê?

As expressões regulares são uma forma de procurar e manipular texto dentro de uma string. Programadores usam expressões regulares para encontrar e substituir padrões específicos de caracteres em grandes conjuntos de dados, economizando tempo e melhorando a eficiência do código.

## Como:

```
// Exemplo 1: Encontrar o número de telefone dentro de uma string
$string = "Meu número de telefone é (123) 456-7890";
$pattern = "/[0-9]{3}([0-9]{3})?-([0-9]{4})/";
preg_match($pattern, $string, $matches);
print_r($matches);

// Output:
// Array ( [0] => 123-456-7890 [1] => 456 [2] => 7890 )
```

```
// Exemplo 2: Substituir todas as vogais em uma string com a letra "x"
$string = "Olá mundo";
$pattern = "/[aeiou]/";
$new_string = preg_replace($pattern, "x", $string);
echo $new_string;

// Output:
// xlx mxndx
```

## Mais detalhes:

As expressões regulares foram criadas na década de 1950 pelo matemático Stephen Cole Kleene, como uma forma de representar padrões em linguagens formais. Além de ser usada em linguagens de programação, também podem ser encontradas em editores de texto e comandos de busca em sistemas operacionais. Em PHP, as expressões regulares são implementadas através da função `preg_match()` para encontrar padrões e `preg_replace()` para substituí-los.

## Veja também:

- [Documentação oficial do PHP sobre expressões regulares](https://www.php.net/manual/pt_BR/book.pcre.php)
- [Tutorial de expressões regulares em PHP](https://www.tutorialspoint.com/php/php_regular_expression.htm)
- [Mais informações sobre a história das expressões regulares](https://en.wikipedia.org/wiki/Regular_expression#History)
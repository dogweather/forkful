---
title:                "PHP: Excluindo caracteres que correspondem a um padrão"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Existe uma necessidade às vezes de remover caracteres de uma string que correspondam a um padrão específico. Isso pode ser útil para limpar dados de entrada ou formatar informações de saída de maneira consistente.

## Como fazer

Para realizar esta tarefa em PHP, podemos usar a função `preg_replace()`. Esta função permite substituir caracteres que correspondam a um padrão por uma string vazia, efetivamente removendo-os.

Veja um exemplo simples abaixo:

```PHP
<?php
$string = "Hello World!";
$nova_string = preg_replace("/[!o]/", "", $string);
echo $nova_string; // resultado: Hell Wrld
?>
```

Neste exemplo, estamos removendo os caracteres "o" e "!" da string original "Hello World!" e imprimindo o resultado. A sintaxe da função `preg_replace()` é `preg_replace($padrao, $substituicao, $string)`, onde `$padrao` especifica o padrão de caracteres a serem removidos e `$substituicao` é a string que será usada para substituir os caracteres correspondentes.

Podemos até mesmo usar expressões regulares mais complexas no padrão para alcançar diferentes tipos de exclusão. Por exemplo, para remover todos os caracteres que não são letras ou números, podemos usar o padrão `/[^A-Za-z0-9]/`:

```PHP
<?php
$string = "123!AbC@";
$nova_string = preg_replace("/[^A-Za-z0-9]/", "", $string);
echo $nova_string; // resultado: 123AbC
?>
```

## Profundando

Para entender melhor como a função `preg_replace()` funciona, precisamos saber sobre expressões regulares. Expressões regulares são padrões de caracteres que correspondem a diferentes tipos de strings. No exemplo acima, usamos a expressão regular `/[!o]/`, que significa "encontre todos os caracteres que sejam '!' ou 'o'". A barra (/) no início e no final do padrão é usada para delimitá-lo.

Existem muitos caracteres especiais que podem ser usados em expressões regulares para alcançar diferentes tipos de correspondência. Por exemplo, `.` corresponde a qualquer caractere, `[a-zA-Z]` corresponde a letras maiúsculas e minúsculas, `+` corresponde a um ou mais ocorrências do padrão anterior e assim por diante.

No PHP, podemos usar expressões regulares em muitas funções além de `preg_replace()`, como `preg_match()` para encontrar correspondências em uma string e `preg_split()` para quebrar uma string em partes com base em um padrão.

## Veja também

- Documentação do PHP sobre [Expressões Regulares](http://php.net/manual/pt_BR/reference.pcre.pattern.syntax.php)
- Tutorial do W3Schools sobre [Expressões Regulares em PHP](https://www.w3schools.com/php/php_regex.asp)
- Tutorial do SitePoint sobre [Expressões Regulares em PHP](https://www.sitepoint.com/expressions-regulaires-php/)
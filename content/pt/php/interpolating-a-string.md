---
title:                "Interpolando uma string"
html_title:           "Java: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O que é e porquê?

Interpolar uma string no PHP significa incluir variáveis diretamente dentro da string, dando-lhe um acesso fácil e rápido aos seus valores. Os programadores fazem isto para evitar concatenar strings, tornando o código mais limpo e eficiente.

## Como fazer:

Aqui estão alguns exemplos de como interpolamos strings com o PHP.

```PHP
$nome = "João";
echo "Olá, $nome! Seja bem-vindo.";
```

Saída:

```
Olá, João! Seja bem-vindo.
```

## Um olhar mais atento

A interpolação de strings não é um conceito único do PHP. Foi introduzido nas linguagens Perl e Shell Script e o PHP adotou-o por ser uma forma eficiente de trabalhar com strings.

Como alternativa à interpolação, pode-se usar a função `sprintf` ou `printf`. No entanto, a interpolação muitas vezes resulta num código mais claro e de leitura fácil.

Note que na interpolação de strings, a string deve ser delimitada por aspas duplas (""). Aspas simples ('') não funcionarão. Isto é diferente de outras linguagens como o JavaScript, em que ambas as aspas simples e duplas funcionam.

```PHP
$nome = "João";
echo 'Olá, $nome! Seja bem-vindo.'; // Isto não vai funcionar, imprime "Olá, $nome! Seja bem-vindo."
```

## Ver também

1. [Interpolação de strings na documentação oficial do PHP](https://www.php.net/manual/pt_BR/language.types.string.php).
2. [Perfis de string e aspas no site PHP: The Right Way](https://phptherightway.com/#strings_and_slangs).
3. [Interpolation or concatenation? A PHP benchmark](https://stackoverflow.com/questions/4679953/single-quotes-vs-double-quotes-in-php-about-speed) for discovering the speed difference between interpolation and concatenation.
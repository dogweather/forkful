---
title:                "Unindo strings"
html_title:           "PHP: Unindo strings"
simple_title:         "Unindo strings"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

Concatenar strings é uma habilidade fundamental em qualquer linguagem de programação, incluindo PHP. Isso permite combinar diferentes partes de strings para criar uma nova string e facilitar a manipulação de dados.

## Como Fazer

A concatenação de strings é bastante simples no PHP, basta usar o operador "." para juntar duas ou mais strings. Veja um exemplo:

```PHP
$primeira_string = "Olá";
$segunda_string = "mundo!";
$string_final = $primeira_string . " " . $segunda_string;

echo $string_final; // Saída: Olá mundo!
```

Neste exemplo, criamos duas strings separadas e depois as juntamos usando o operador "." com um espaço extra para criar uma nova string.

Outra forma de concatenar strings é usar a função `concat`, que também funciona com arrays. Veja um exemplo:

```PHP
$array_strings = array("Este", "é", "um", "array", "de", "strings");
$string_final = implode(" ", $array_strings);

echo $string_final; // Saída: Este é um array de strings
```

## Deep Dive

Quando concatenamos strings, é importante ficar atento à ordem em que as juntamos. Por exemplo, se o objetivo é criar um endereço de e-mail combinando o nome e o sobrenome de uma pessoa, é necessário prestar atenção à adição de espaços extras ou caracteres de pontuação:

```PHP
$primeiro_nome = "Maria";
$ultimo_nome = "Silva";
$email = $primeiro_nome . "." . $ultimo_nome . "@exemplo.com";

echo $email; // Saída: Maria.Silva@exemplo.com
```

No exemplo acima, usamos o operador "." para juntar o primeiro nome, ponto e sobrenome, criando um endereço de e-mail válido.

Também é importante lembrar que a concatenação de strings pode ser usada não apenas para criar novas strings, mas também para adicionar conteúdo a uma string existente. Podemos usar o operador ".=" para adicionar uma string a outra, como no exemplo a seguir:

```PHP
$string = "Olá";
$string .= " mundo!";

echo $string; // Saída: Olá mundo!
```

No código acima, a segunda string é adicionada à primeira, resultando em uma única string concatenada.

## Veja Também

- [Documentação Oficial do PHP sobre Concatenação de Strings](https://www.php.net/manual/pt_BR/language.operators.string.php)
- [Artigo sobre Strings no PHP](https://www.devmedia.com.br/strings-no-php-concatenacao-tamanho-busca-replace/37586) (em português)
- [Tutorial de PHP para Iniciantes](https://www.hostinger.com.br/tutoriais/o-que-e-php) (em português)
---
title:    "PHP: Extraindo substrings"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por que extrair substrings é útil?

A extração de substrings é útil para obter partes específicas de uma string sem precisar manipular a string original. Isso pode ser útil em várias situações de programação, como manipulação de texto, validação de dados, entre outros.

## Como extrair substrings em PHP

Para extrair substrings em PHP, podemos usar a função `substr()`, que recebe a string original, o índice inicial e opcionalmente o tamanho da substring desejada como parâmetros. Veja um exemplo abaixo:

```PHP
$string = "Olá, mundo!";
$sub = substr($string, 5, 5);
echo $sub; // saída: mundo
```

Podemos também usar índices negativos para extrair substrings a partir do final da string. Por exemplo:

```PHP
$string = "Olá, mundo!";
$sub = substr($string, -6, 6);
echo $sub; // saída: mundo!
```

## Detalhes sobre a extração de substrings

Além do índice inicial, podemos também passar para a função `substr()` um terceiro parâmetro opcional: o tamanho da substring desejada. Isso é útil quando queremos extrair uma parte específica da string, mas não queremos cortar a string no final. Por exemplo:

```PHP
$string = "Programando em PHP";
$sub = substr($string, 12, 2);
echo $sub; // saída: em
```

Também podemos passar o terceiro parâmetro como um número negativo, que será interpretado como a posição a partir do final da string. Por exemplo:

```PHP
$string = "Programando em PHP";
$sub = substr($string, -3, 3);
echo $sub; // saída: PHP
```

Além disso, a função `substr()` é sensível ao tipo de codificação utilizado na string original. Portanto, ao manipular strings com caracteres multibyte (como o UTF-8), é importante garantir que a função esteja devidamente configurada para isso.

## Veja também

- Documentação oficial do PHP sobre a função `substr()`: https://www.php.net/manual/pt_BR/function.substr.php
- Artigo sobre manipulação de strings em PHP: https://www.devmedia.com.br/manipulando-strings-em-php/40753
- Tutorial sobre validação de strings em PHP: https://www.devmedia.com.br/validando-strings-em-php/40223
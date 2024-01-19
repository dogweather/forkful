---
title:                "Encontrando o comprimento de uma string"
html_title:           "C: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Encontrando o Comprimento de uma String em PHP

## O Que & Por Quê?

Determinar o comprimento de uma string significa saber quantos caracteres existem nela. Isso é útil em diversas situações, como validação de entrada do usuário, manipulação de texto e controle de fluxo do programa.

## Como fazer:

Use a função `strlen()` para encontrar o comprimento de uma string. Aqui está um exemplo:

```PHP
$texto = "Olá, Mundo!";
$comprimento = strlen($texto);
echo $comprimento; // Saída: 12
```

Neste exemplo, a string "Olá, Mundo!" tem 12 caracteres, incluindo espaços e pontuação.

## Mergulho Profundo

A função `strlen()` existe desde as primeiras versões do PHP e é a maneira padrão de se encontrar o comprimento de uma string em PHP. Ela conta o número de bytes da string, não o número de caracteres. Isto é importante quando você está trabalhando com caracteres multibyte (como os usados em alguns idiomas não latinos).

Se precisar lidar com caracteres multibyte, use a função `mb_strlen()`. 

```PHP
$texto = "çäö";
$comprimento = strlen($texto);
$comprimento_mb = mb_strlen($texto);
echo $comprimento; // Saída: 6
echo $comprimento_mb; // Saída: 3
```

Neste exemplo, `strlen()` retorna 6 porque cada caracter especial "çäö" tem 2 bytes. Mas, `mb_strlen()` retorna 3, o que é o número real de caracteres.

## Veja Também

1. Documentação oficial do PHP para `strlen()`: [https://www.php.net/manual/pt_BR/function.strlen.php](https://www.php.net/manual/pt_BR/function.strlen.php)
2. Documentação oficial do PHP para `mb_strlen()`: [https://www.php.net/manual/pt_BR/function.mb-strlen.php](https://www.php.net/manual/pt_BR/function.mb-strlen.php)
3. Um guia mais aprofundado sobre strings em PHP: [https://www.tutorialrepublic.com/php-tutorial/php-strings.php](https://www.tutorialrepublic.com/php-tutorial/php-strings.php)
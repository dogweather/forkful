---
title:                "Encontrando o comprimento de uma string."
html_title:           "PHP: Encontrando o comprimento de uma string."
simple_title:         "Encontrando o comprimento de uma string."
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Porque

Saber o comprimento de uma string é uma tarefa básica em programação e pode ser útil em várias situações, como validar entradas de usuários, formatar saídas de texto e realizar operações com strings.

## Como Fazer

Para encontrar o comprimento de uma string em PHP, podemos usar a função `strlen()` ou acessar a propriedade `length` de um objeto `string`. Veja abaixo exemplos de como utilizar esses métodos:

```
// Exemplo com a função strlen()
$string = "Olá Mundo!";
echo strlen($string); // Saída: 11

// Exemplo com a propriedade length
$string = "Olá PHP!";
echo $string->length; // Saída: 8
```

Note que a função `strlen()` retorna o comprimento da string passada como parâmetro, enquanto a propriedade `length` retorna o comprimento daquela string específica em que ela é utilizada.

## Deep Dive

Quando uma string é passada como parâmetro para a função `strlen()`, o PHP conta a partir do primeiro caractere da string até o último, incluindo espaços em branco e caracteres especiais. Isso significa que a função não diferencia entre letras minúsculas e maiúsculas ou caracteres especiais.

Outra forma de encontrar o comprimento de uma string é utilizando a função `mb_strlen()`, que leva em consideração a codificação dos caracteres. Isso é importante para lidar com strings em outras línguas, que podem ter caracteres especiais ou acentuados.

```
// Exemplo com a função mb_strlen()
$string = "Água é vida!";
echo mb_strlen($string, "UTF-8"); // Saída: 12
```

## Veja Também

- [Documentação oficial do PHP sobre a função strlen()](https://www.php.net/manual/pt_BR/function.strlen.php)
- [Documentação oficial do PHP sobre a função mb_strlen()](https://www.php.net/manual/pt_BR/function.mb-strlen.php)
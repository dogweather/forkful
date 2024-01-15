---
title:                "Extraindo subtrings"
html_title:           "PHP: Extraindo subtrings"
simple_title:         "Extraindo subtrings"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair subcadeias?

Extrair subcadeias de uma string é uma tarefa comum na programação, especialmente em PHP, pois permite que você obtenha informações específicas de uma string maior. Isso pode ser útil em várias situações, como extrair um número de telefone de uma entrada de formulário ou obter o sobrenome de um nome completo.

## Como fazer

Extrair subcadeias em PHP é feito por meio da função `substr()`. Veja um exemplo básico de como usá-la:

```PHP
$string = "Este é um exemplo de string.";

// Extraindo uma subcadeia a partir do quinto caractere até o final da string
$nova_subcadeia = substr($string, 4);

echo $nova_subcadeia; // Output: é um exemplo de string.
```

Você também pode especificar um ponto de partida e o comprimento da subcadeia que deseja extrair:

```PHP
$string = "Este é um exemplo de string.";

// Extraindo uma subcadeia iniciando a partir do quinto caractere e com comprimento de 10 caracteres
$nova_subcadeia = substr($string, 4, 10);

echo $nova_subcadeia; // Output: é um exem
```

Note que os caracteres são contados a partir do zero, então o quinto caractere seria o número 4. Além disso, se o comprimento especificado for maior do que a quantidade de caracteres restantes na string, o restante da string será retornado.

Você também pode passar um valor negativo para indicar a posição a partir da qual a subcadeia deve ser extraída a partir do final da string:

```PHP
$string = "Este é um exemplo de string.";

// Extraindo uma subcadeia iniciando a partir do quinto caractere do final e com comprimento de 10 caracteres
$nova_subcadeia = substr($string, -5, 10);

echo $nova_subcadeia; // Output: string.
```

## Mergulho profundo

Além dos parâmetros básicos da função `substr()`, existem outros recursos que podem ser úteis ao extrair subcadeias em PHP.

Por exemplo, se você não tiver certeza do comprimento da string a ser extraída, pode usar a função `strlen()` para obtê-lo dinamicamente:

```PHP
$string = "Este é um exemplo de string.";

// Extraindo uma subcadeia iniciando a partir do quinto caractere e com comprimento dinâmico
$nova_subcadeia = substr($string, 4, strlen($string) - 4);

echo $nova_subcadeia; // Output: é um exemplo de string.
```

Além disso, você pode usar o operador de atribuição combinada (`.=`) para adicionar a subcadeia extraída a uma variável existente, em vez de criar uma nova variável.

## Veja também

Aqui estão alguns recursos adicionais sobre a função `substr()` e como usá-la em PHP:

- Documentação oficial do PHP sobre a função `substr()`: https://www.php.net/manual/en/function.substr.php
- Artigo do W3Schools sobre a função `substr()` em PHP: https://www.w3schools.com/php/func_string_substr.asp
---
title:    "PHP: Extraindo Substrings"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings em programação

Extrair substrings, ou partes de uma string, é um processo importante na programação PHP. Isso permite que você manipule e processe informações específicas dentro de uma string maior. Seja para formatar dados, buscar informações específicas ou qualquer outra necessidade, a extração de substrings é uma habilidade valiosa para os programadores.

## Como extrair substrings em PHP

Extrair substrings em PHP é bastante simples. Você pode usar a função `substr()` seguida de três argumentos: a string original, o índice inicial e a quantidade de caracteres a serem extraídos.

```
$original = "Esta é uma string de exemplo";
$substring = substr($original, 8, 6);

echo $substring; // saída: "string"
```

Neste exemplo, definimos a string original como "Esta é uma string de exemplo" e extraímos uma substring começando a partir do 8º caractere, que é "s", e continuando por 6 caracteres, resultando em "string". Você também pode extrair substrings contando da direita para a esquerda, usando um índice negativo.

```
$original = "Esta é uma string de exemplo";
$substring = substr($original, -7, 3);

echo $substring; // saída: "emp"
```

Note que a contagem de índice começa em 0, então o primeiro caractere é 0, o segundo é 1, e assim por diante. Além disso, se você omitir o terceiro argumento, a substring será extraída até o final da string original.

## Mergulho profundo na extração de substrings

Existem alguns detalhes importantes a serem considerados ao extrair substrings em PHP. Em primeiro lugar, se o índice inicial for maior que o tamanho da string, a função `substr()` retornará uma string vazia. Além disso, se o índice inicial for um número negativo e maior do que o tamanho da string, a função irá começar a extração contando do início da string original.

Você também pode utilizar a função `mb_substr()` para extrair substrings em PHP com suporte a caracteres multibyte, que incluem caracteres com acentos e símbolos. Esta função possui os mesmos três argumentos que a `substr()`, mas certifique-se de habilitar o suporte a caracteres multibyte em seu PHP.ini.

## Veja também

- Documentação oficial sobre a função `substr()`: https://www.php.net/manual/en/function.substr.php
- Documentação oficial sobre a função `mb_substr()`: https://www.php.net/manual/en/function.mb-substr.php 
- Tutorial sobre strings em PHP: https://www.php.net/manual/en/language.types.string.php
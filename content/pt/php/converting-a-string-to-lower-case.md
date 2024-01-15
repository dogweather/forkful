---
title:                "Convertendo uma string para minúsculas"
html_title:           "PHP: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que converter uma string para minúsculas?

Converter uma string para minúsculas pode ser útil em várias situações, seja para padronizar a entrada de dados em um formulário, para fazer comparações de strings sem diferenciar maiúsculas e minúsculas ou simplesmente por preferência de formatação.

## Como fazer a conversão em PHP

Usar a função `strtolower()` é a maneira mais simples e rápida de converter uma string para letras minúsculas. Veja um exemplo de código abaixo:

```PHP
$string = "EXEMPLO DE STRING EM LETRAS MAIÚSCULAS";
echo strtolower($string);
```

**Saída:**
```
exemplo de string em letras maiúsculas
```

Além disso, também é possível fazer a conversão utilizando a função `mb_strtolower()`, que suporta caracteres multibyte (como acentos). Veja um exemplo de código:

```PHP
$string = "EXEMPLO DE STRING COM CARACTERES MULTIBYTE ÁÉÍÓÚ";
echo mb_strtolower($string);
```

**Saída:**
```
exemplo de string com caracteres multibyte áéíóú
```

## Aprofundando na conversão de string para minúsculas

Ao converter uma string para minúsculas, é importante ter em mente que a função `strtolower()` considera a tabela ASCII para fazer a conversão. Isso significa que não irá funcionar corretamente com letras acentuadas ou caracteres multibyte.

Por isso, é recomendado utilizar a função `mb_strtolower()` se você estiver lidando com strings que contenham caracteres multibyte. Além disso, é importante também estar atento ao conjunto de caracteres (charset) utilizado, para evitar possíveis problemas de exibição.

Em alguns casos, pode ser necessário fazer a conversão reversa, ou seja, de minúsculas para maiúsculas. Para isso, basta utilizar a função `strtoupper()` ou `mb_strtoupper()` de maneira semelhante às funções mencionadas anteriormente.

## Veja também

- Documentação oficial do PHP sobre a função [`strtolower()`](https://www.php.net/manual/pt_BR/function.strtolower.php)
- Documentação oficial do PHP sobre a função [`mb_strtolower()`](https://www.php.net/manual/pt_BR/function.mb-strtolower.php)
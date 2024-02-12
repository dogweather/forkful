---
title:                "Capitalizando uma string"
date:                  2024-02-03T19:05:55.318998-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizando uma string"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Quê & Por Quê?
Capitalizar uma string envolve modificar o primeiro caractere de um texto dado para maiúsculo, garantindo que frases, títulos ou nomes próprios comecem corretamente em um conjunto de dados. Programadores frequentemente executam a capitalização de strings para a normalização de dados, melhorando a legibilidade ou garantindo consistência na entrada de usuário ou no processamento de dados textuais.

## Como fazer:
O PHP suporta nativamente várias funções para capitalizar strings, cada uma servindo a um propósito diferente. Veja como você pode usá-las:

### Capitalizando a primeira letra de uma string:

```php
$string = "hello, world!";
$capitalizedString = ucfirst($string);
echo $capitalizedString; // Exibe: Hello, world!
```

### Capitalizando a primeira letra de cada palavra:

```php
$string = "hello, world!";
$capitalizedWords = ucwords($string);
echo $capitalizedWords; // Exibe: Hello, World!
```

### Convertendo toda a string para maiúsculo:

```php
$string = "hello, world!";
$upperCaseString = strtoupper($string);
echo $upperCaseString; // Exibe: HELLO, WORLD!
```

Para cenários que requerem mais personalização ou soluções de terceiros, bibliotecas como `mbstring` (para strings multibyte) podem ser utilizadas, especialmente quando lidando com internacionalização onde caracteres podem se estender além do conjunto básico ASCII.

### Usando mbstring para capitalizar strings UTF-8:

Certifique-se de ter a extensão `mbstring` habilitada na sua configuração PHP, então:

```php
$string = "élégant";
$capitalizedString = mb_convert_case($string, MB_CASE_TITLE, "UTF-8");
echo $capitalizedString; // Exibe: Élégant
```

Esta abordagem ajuda a capitalizar com precisão strings que incluem caracteres não-ASCII, aderindo às nuances de várias línguas.

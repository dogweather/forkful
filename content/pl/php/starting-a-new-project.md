---
title:                "Rozpoczynanie nowego projektu"
date:                  2024-01-20T18:04:10.110164-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rozpoczynanie nowego projektu"
programming_language: "PHP"
category:             "PHP"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zaczynając nowy projekt, programiści tworzą grunt pod swój kod - to jak ułozenie fundamentów pod dom. Robią to, bo dobrze przygotowany start to klucz do elastycznego i efektywnego rozwoju aplikacji.

## Jak to zrobić:
Załóżmy, że zaczynamy projekt w PHP. Oto jak możesz to zrobić:

```PHP
<?php
// Zamysł: prosty skrypt PHP jako punkt wyjścia dla nowego projektu

echo "Witaj w nowym projekcie PHP!"; // Wyświetla powitanie

// Tutaj będziesz rozszerzał swoją aplikację...
?>
```
Po uruchomieniu kodu otrzymasz:

```
Witaj w nowym projekcie PHP!
```

Dla bardziej skomplikowanego projektu użyjemy Composera - menadżera zależności dla PHP:

```bash
composer init
composer require symfony/http-foundation
```

A potem twórz swoją aplikację w oparciu o te biblioteki.

## Głębsze spojrzenie:
Na przestrzeni lat sposób tworzenia nowych projektów w PHP ewoluował. Na początku, bez narzędzi takich jak Composer, kodowano wszystko od zera. Composer, wydany w 2012 roku, umożliwił zarządzanie bibliotekami i zależnościami projektu, co było olbrzymią zmianą.

Inne narzędzia jak PHP Frameworks (np. Symfony, Laravel), oferują gotowe komponenty podstawowe. Umożliwiają one tworzenie aplikacji w sposób bardziej zorganizowany i szybki, dzięki wykorzystaniu gotowych rozwiązań.

## Zobacz również:
- [Composer](https://getcomposer.org/doc/)
- [Symfony](https://symfony.com/doc/current/setup.html)
- [Laravel](https://laravel.com/docs/)

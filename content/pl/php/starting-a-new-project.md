---
title:                "Rozpoczynanie nowego projektu"
html_title:           "Bash: Rozpoczynanie nowego projektu"
simple_title:         "Rozpoczynanie nowego projektu"
programming_language: "PHP"
category:             "PHP"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Zaczynanie nowego projektu programistycznego to proces tworzenia unikalnej aplikacji od podstaw. Programiści robią to, aby rozwiązać specyficzne problemy lub ułatwić ludziom życie za pomocą technologii.

## Jak to zrobić:

Zacznijmy od prostego przykładu - "witaj, świecie" w PHP.
```PHP
<?php
echo "Witaj, świecie!";
?>
```
Gdy uruchomisz ten kod, twoje wyjście powinno wyglądać tak:
```PHP
Witaj, świecie!
```
Teraz, zaczniemy bardziej skomplikowany projekt: prosta aplikacja do zarządzania kontaktami. Będziemy potrzebować pliku `index.php`.

```PHP
<?php
$contacts = array(
    array("name" => "Anna", "phone" => "123-456-7890"),
    array("name" => "Tomasz", "phone" => "098-765-4321"),
);

foreach ($contacts as $contact) {
    echo "Name: " . $contact["name"] . ", Phone: " . $contact["phone"] . "\n";
}
?>
```
Staraj się za każdym razem tworzyć modularne i łatwe do testowania kody.

## W głąb tematu:

Zaczynanie nowego projektu programistycznego ma swoje korzenie w procesie twórczym, zdecydowanie dalej od czysto technicznego. W latach '70 i '80, programiści często musieli zaczynać nowe projekty od niczego.

Istnieje wiele alternatyw dla PHP, takich jak Python, Ruby czy JavaScript. Wybór języka zależy od celów projektu i preferencji programisty.

Oprócz korzystania z "gołego" PHP, innym popularnym podejściem jest korzystanie z frameworków, takich jak Laravel, Symfony lub CodeIgniter, które ułatwiają i przyspieszają proces budowy aplikacji.

## Zobacz także:

3. [Podstawy programowania w PHP](https://www.codecademy.com/learn/learn-php)
---
title:                "Praca z formatem json"
html_title:           "PHP: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego?

JSON (JavaScript Object Notation) jest jednym z najpowszechniej używanych formatów danych w dzisiejszych aplikacjach internetowych. Jest on bardzo lekki, czytelny dla ludzi i łatwy do przetwarzania przez komputery. W artykule tym opiszemy, dlaczego warto nauczyć się pracować z JSON w PHP.

## Jak to zrobić?

W celu pracy z formatem JSON w PHP, musimy użyć wbudowanych funkcji języka lub zewnętrznych bibliotek. Przed przystąpieniem do pracy z danymi JSON, należy pamiętać o sprawdzeniu, czy plik jest poprawnym obiektem JSON. W przeciwnym razie może to spowodować błędy w naszym kodzie.

### Przetwarzanie danych JSON przy użyciu wbudowanych funkcji PHP

Aby przetworzyć dane JSON przy użyciu wbudowanych funkcji PHP, musimy najpierw przekonwertować je na obiekt lub tablicę przy użyciu funkcji `json_decode()`. Następnie możemy wykorzystać syntaktykę obiektową lub tablicową, aby uzyskać dostęp do poszczególnych elementów danych.

```PHP
$json = '{"imie": "Anna", "wiek": 30, "hobby": ["sport", "muzyka", "podróże"]}';

$osoba = json_decode($json);

echo $osoba->imie; // wyświetli "Anna"
echo $osoba->wiek; // wyświetli 30
echo $osoba->hobby[0]; // wyświetli "sport"
```
### Przetwarzanie danych JSON przy użyciu zewnętrznej biblioteki

Jeśli chcemy wykorzystać bardziej zaawansowane funkcje do pracy z danymi JSON, możemy użyć zewnętrznej biblioteki, takiej jak `Symfony Serializer` lub `JSON-PHP`. Przykładowy kod przy użyciu biblioteki `Symfony Serializer` może wyglądać następująco:

```PHP
require_once 'vendor/autoload.php';

use Symfony\Component\Serializer\Serializer;

$json = '{"imie": "Anna", "wiek": 30, "hobby": ["sport", "muzyka", "podróże"]}';

$serializer = new Serializer();
$osoba = $serializer->deserialize($json, 'App\Entity\Osoba', 'json');

echo $osoba->getImie(); // wyświetli "Anna"
echo $osoba->getWiek(); // wyświetli 30
echo $osoba->getHobby()[0]; // wyświetli "sport"
```

## Deep Dive

Gdy już nauczysz się podstaw pracy z formatem JSON w PHP, możesz rozszerzyć swoją wiedzę o bardziej zaawansowane techniki, takie jak manipulowanie danymi, iteracja po obiektach lub dokonywanie złożonych zapytań. Ponadto, w przypadku tworzenia aplikacji internetowych, warto zwrócić uwagę na bezpieczeństwo danych JSON i zabezpieczenie ich przed nieautoryzowanym dostępem.

## Zobacz także

- [Dokumentacja PHP: JSON](https://www.php.net/manual/pl/book.json.php)
- [Biblioteka Symfony Serializer](https://symfony.com/doc/current/components/serializer.html)
- [JSON-PHP](https://github.com/pear/Net_JSON)
- [Bezpieczeństwo - OWASP Foundation](https://owasp.org/www-project-api-security/)
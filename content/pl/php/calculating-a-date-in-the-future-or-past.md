---
title:                "Obliczanie daty w przyszłości lub przeszłości."
html_title:           "PHP: Obliczanie daty w przyszłości lub przeszłości."
simple_title:         "Obliczanie daty w przyszłości lub przeszłości."
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O co chodzi i dlaczego?

Obliczanie daty w przyszłości lub przeszłości jest powszechnym zadaniem w programowaniu. Polega ono na matematycznym manipulowaniu datami w celu uzyskania konkretnej daty w przyszłości lub przeszłości. Programiści często muszą wykonywać takie obliczenia, aby np. wyświetlić datę ważnego wydarzenia lub określić okres trwania projektu.

## Jak to zrobić?

Kodowanie obliczania daty w przyszłości lub przeszłości jest stosunkowo łatwe w języku PHP. Poniższe przykłady pokazują kilka sposobów na to, jak można to zrobić:

```PHP
// Ustawiamy datę na dzień dzisiejszy
$dzis = new DateTime();
echo 'Dzisiaj jest ' . $dzis->format('d/m/Y') . PHP_EOL;

// Obliczamy datę, która będzie za rok
$jutro = new DateTime('+1 year');
echo 'Za rok będzie ' . $jutro->format('d/m/Y') . PHP_EOL;

// Obliczamy datę, która będzie za dwa tygodnie
$za_dwa_tygodnie = new DateTime('+2 weeks');
echo 'Za dwa tygodnie będzie ' . $za_dwa_tygodnie->format('d/m/Y') . PHP_EOL;
```

Wynikiem wykonania powyższego kodu będzie:

```
Dzisiaj jest 22/07/2021
Za rok będzie 22/07/2022
Za dwa tygodnie będzie 05/08/2021
```

## W głębi sprawy

Obliczanie daty w przyszłości lub przeszłości ma długą historię, sięgającą czasów starożytnych. Wtedy ludzie musieli opierać się na obserwacjach astronomicznych, aby określić datę. W dzisiejszych czasach jest to znacznie prostsze dzięki zaawansowanym funkcjom dostępnym w językach programowania.

Alternatywnym sposobem na obliczanie daty w przyszłości lub przeszłości jest użycie biblioteki DateTime, która oferuje szereg funkcji do pracy z datami. Można także użyć funkcji date() w połączeniu z funkcją strtotime() w celu obliczenia daty w konkretnym formacie.

## Zobacz także

- Oficjalna dokumentacja PHP dotycząca klasy DateTime (https://www.php.net/manual/en/class.datetime.php)
- Przykładowe funkcje DateTime w PHP (https://www.w3schools.com/php/php_ref_date.asp)
---
title:                "PHP: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwertowanie tekstu na małe litery jest częstym zadaniem w programowaniu. Jest to szczególnie ważne, jeśli pracujemy z danymi użytkowników, ponieważ chcemy, aby nasze aplikacje były odporniejsze na błędy i łatwiejsze w obsłudze. W tym artykule dowiesz się, dlaczego jest tak ważne konwertowanie tekstu na małe litery i jak możesz to zrobić w języku PHP.

## Jak to zrobić

W języku PHP istnieje wiele sposobów na konwertowanie tekstu na małe litery. Najprostszym i najczęściej stosowanym sposobem jest użycie funkcji `strtolower()`. Przyjmuje ona jako argument tekst, który chcemy przekonwertować, a następnie zwraca tekst w postaci małych liter. Poniżej przedstawiam przykładowy kod używający tej funkcji:

```PHP
$text = "PRzyKłaDowY tEkST dO kOnWeRtOwAnIA";
echo strtolower($text);
```

**Wynik:**

`przykładowy tekst do konwertowania`

Jeśli chcemy przekonwertować tylko pierwszą literę tekstu, możemy użyć funkcji `ucfirst()`, która zmienia tylko pierwszą literę na dużą lub `lcfirst()`, która zmienia tylko pierwszą literę na małą.

## Deep Dive

Konwertowanie tekstu na małe litery może być przydatne również w walidacji danych użytkownika. Dzięki temu możemy zapewnić, że nie będzie błędów w nazwach użytkownika czy adresach email, które często są podawane wielkimi literami przez użytkowników.

W języku PHP możemy również skorzystać z funkcji `mb_strtolower()`, która jest bardziej zaawansowaną wersją funkcji `strtolower()`, ponieważ uwzględnia ona znaki specjalne i sprowadza je do małych liter zgodnie z ustawieniami kodowania znaków.

Podczas konwertowania tekstu na małe litery warto również pamiętać o ustawieniach regionalnych (locale) i ich wpływie na wynik końcowy.

## Zobacz również

- Dokumentacja PHP o funkcji `strtolower()`: https://www.php.net/manual/en/function.strtolower.php
- Dokumentacja PHP o funkcji `mb_strtolower()`: https://www.php.net/manual/en/function.mb-strtolower.php
- Wpływ ustawień regionalnych na konwertowanie tekstu: https://www.php.net/manual/en/function.setlocale.php
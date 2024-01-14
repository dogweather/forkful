---
title:                "PHP: Wyodrębnianie podciągów"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy zastanawiałeś się kiedykolwiek, jak możesz wyodrębnić część tekstu ze swojego kodu w PHP? Może chcesz wyświetlić tylko imię i nazwisko użytkownika bez całego adresu email, który jest przechowywany w bazie danych? W takim przypadku, funkcja PHP do wyodrębniania podciągów może okazać się bardzo przydatna.

## Jak to zrobić

Aby wyodrębnić podciąg z tekstu w PHP, możesz skorzystać z funkcji `substr()`. Przyjmujemy, że mamy zmienną `$text` zawierającą pełną informację o użytkowniku w formacie "Imię Nazwisko (email)". W takim przypadku, możemy użyć `substr($text, 0, strpos($text, '(') - 1)` aby wyodrębnić tylko imię i nazwisko, a pomińmiemy część po nawiasie "(".

Możemy również użyć `substr($text, strpos($text, '(') + 1, -1)` aby wyodrębnić tylko email, pomijając część przed i po nawiasie "(" i ")".

Poniżej znajdują się przykłady użycia funkcji `substr()` oraz ich oczekiwany wynik:

```PHP
<?php
$text = "Jan Kowalski (jan_kowalski@gmail.com)";
echo substr($text, 0, strpos($text, '(') - 1); // Jan Kowalski
echo substr($text, strpos($text, '(') + 1, -1); // jan_kowalski@gmail.com
```

## Deep Dive

Funkcja `substr()` pozwala na wyodrębnienie części tekstu na podstawie podanego indeksu początkowego i końcowego. Możemy również użyć wartości ujemnych dla indeksów, co pozwala na liczenie od końca tekstu.

Na przykład, `substr($text, -7)` zwróci ostatnie 7 znaków tekstu, a `substr($text, -11, 4)` zwróci 4 kolejne znaki, zaczynając od 11 znaku od końca.

Warto również wspomnieć o funkcji `strlen()`, która pozwala na odczytanie długości tekstu. Jest ona przydatna przy wyodrębnianiu podciągów, gdy nie znamy dokładnego indeksu końcowego.

## Zobacz również

Chcesz dowiedzieć się więcej o wyodrębnianiu podciągów w PHP? Sprawdź poniższe linki:

- [Oficjalna dokumentacja PHP](https://www.php.net/manual/en/function.substr.php)
- [Tutorial na W3Schools](https://www.w3schools.com/php/func_string_substr.asp)
- [Przykładowe zastosowania funkcji substr()](https://www.tabnine.com/code/php/functions/substr)
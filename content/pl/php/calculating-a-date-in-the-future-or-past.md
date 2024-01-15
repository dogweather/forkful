---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "PHP: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Kalkulowanie daty w przyszłości lub przeszłości może być przydatne w wielu różnych scenariuszach. Na przykład, jeśli tworzysz aplikację planującą, może być konieczne przewidywanie daty wydarzenia lub wystawienia faktury w przyszłości. W ten sposób można automatycznie wygenerować wyszukiwane informacje, zapewniając wygodę dla użytkownika.

## Jak to zrobić

Aby obliczyć datę w przyszłości lub przeszłości w PHP, możemy użyć wbudowanej funkcji `date()`. Ta funkcja pobiera dwa argumenty: `format` i `timestamp`. Pierwszy określa sposób prezentacji daty, a drugi określa datę, dla której chcemy obliczyć nową datę w przeszłości lub przyszłości.

Przykładowe użycie:

```PHP
$today = date('Y-m-d'); // bieżąca data, format: rok-miesiąc-dzień
$future_date = date('Y-m-d', strtotime('+1 week')); // data w przyszłości, dokładnie za tydzień

echo "Dzisiejsza data: $today";
echo "Data za tydzień: $future_date";
```

W powyższym przykładzie użyliśmy funkcji `strtotime()` do obliczenia daty w przyszłości. Funkcja ta interpretuje podany ciąg znaków jako datę i zwraca jej wartość w formacie `timestamp`. W ten sposób możemy wygodnie wykorzystać różne wartości, takie jak "1 week" czy "next month".

## Deep Dive

W przypadku bardziej zaawansowanych obliczeń związanych z datami, PHP oferuje również dodatkowe funkcje, takie jak `DateTime` i `DateInterval`. Dzięki nim można wykonać dokładniejsze i bardziej skomplikowane operacje, takie jak przewidywanie daty wraz z uwzględnieniem różnych stref czasowych czy dni roboczych.

Na przykład, aby obliczyć date wystawienia faktury 10 dni roboczych po bieżącym dniu, można użyć kodu:

```PHP
$today = new DateTime(); // tworzymy obiekt DateTime reprezentujący bieżącą datę
$due_date = $today->add(new DateInterval('P10D')); // dodajemy interwał 10 dni do bieżącej daty

echo "Data wystawienia faktury: " . $today->format('Y-m-d');
echo "Data zapłaty: " . $due_date->format('Y-m-d');
```

W powyższym przykładzie użyliśmy `DateTime` oraz `DateInterval` do obliczenia daty zapłaty dla faktury wystawionej 10 dni roboczych później.

## Zobacz również

- [Dokumentacja PHP: Function.date](https://www.php.net/manual/en/function.date.php)
- [Dokumentacja PHP: Class.datetime](https://www.php.net/manual/en/class.datetime.php)
- [Dokumentacja PHP: Class.dateinterval](https://www.php.net/manual/en/class.dateinterval.php)
---
title:                "Analiza składniowa daty z łańcucha znaków"
date:                  2024-02-03T19:15:05.014970-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analiza składniowa daty z łańcucha znaków"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Parsowanie daty z ciągu znaków w PHP polega na konwersji tekstu, który przedstawia datę i/lub czas, na obiekt `DateTime` PHP lub inne formaty daty/czasu. Jest to kluczowe dla celów walidacji danych, manipulacji, przechowywania i prezentacji, szczególnie podczas pracy z danymi użytkownika lub danymi z zewnętrznych źródeł.

## Jak to zrobić:

Wbudowana klasa `DateTime` w PHP oferuje potężny zestaw funkcji do parsowania i pracy z datami. Można utworzyć instancję `DateTime` z ciągu daty za pomocą konstruktora, a następnie formatować ją według potrzeb. Oto jak:

```php
$dateString = "2023-04-25 15:30:00";
$dateObject = new DateTime($dateString);

echo $dateObject->format('Y-m-d H:i:s');
// Wynik: 2023-04-25 15:30:00
```

Aby obsłużyć ciągi, które stosują niestandardowe formaty, można użyć metody `createFromFormat`, która pozwala określić dokładny format daty wejściowej:

```php
$dateString = "25-04-2023 3:30 PM";
$dateObject = DateTime::createFromFormat('d-m-Y g:i A', $dateString);

echo $dateObject->format('Y-m-d H:i:s');
// Wynik: 2023-04-25 15:30:00
```

Dla bardziej skomplikowanego parsowania, które może nie być bezpośrednio obsługiwane przez `DateTime`, PHP oferuje funkcję `strtotime`, która próbuje przekształcić dowolny angielski opis tekstu daty/czasu na znacznik czasu Unix:

```php
$timestamp = strtotime("next Thursday");
echo date('Y-m-d', $timestamp);
// Wynik będzie się różnić w zależności od aktualnej daty, np. "2023-05-04"
```

**Korzystanie z bibliotek stron trzecich:**

Chociaż wbudowane funkcje PHP obejmują szereg przypadków użycia, czasami może być potrzebna bardziej zaawansowana funkcjonalność parsowania. Biblioteka Carbon, rozszerzenie klasy DateTime PHP, oferuje bogaty zestaw funkcji do manipulacji datą/czasem:

```php
require 'vendor/autoload.php';

use Carbon\Carbon;

$dateString = "Tomorrow";
$date = Carbon::parse($dateString);

echo $date->toDateTimeString();
// Wynik będzie się różnić, np. "2023-04-26 00:00:00"
```

Metoda `parse` w Carbonie inteligentnie radzi sobie z wieloma formatami daty i czasu, co czyni ją nieocenionym narzędziem dla aplikacji wymagających elastycznej funkcjonalności parsowania daty.

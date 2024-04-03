---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:05.014970-07:00
description: "Jak to zrobi\u0107: Wbudowana klasa `DateTime` w PHP oferuje pot\u0119\
  \u017Cny zestaw funkcji do parsowania i pracy z datami. Mo\u017Cna utworzy\u0107\
  \ instancj\u0119 `DateTime` z ci\u0105gu\u2026"
lastmod: '2024-03-13T22:44:35.506813-06:00'
model: gpt-4-0125-preview
summary: "Wbudowana klasa `DateTime` w PHP oferuje pot\u0119\u017Cny zestaw funkcji\
  \ do parsowania i pracy z datami."
title: "Analiza sk\u0142adniowa daty z \u0142a\u0144cucha znak\xF3w"
weight: 30
---

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

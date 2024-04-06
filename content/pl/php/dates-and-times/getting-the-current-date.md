---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:30.944924-07:00
description: "Jak to zrobi\u0107: Wbudowana funkcja `date()` w PHP jest najbardziej\
  \ bezpo\u015Brednim sposobem na pobranie bie\u017C\u0105cej daty. Mo\u017Cna formatowa\u0107\
  \ dat\u0119 na r\xF3\u017Cne sposoby,\u2026"
lastmod: '2024-03-13T22:44:35.507885-06:00'
model: gpt-4-0125-preview
summary: "Wbudowana funkcja `date()` w PHP jest najbardziej bezpo\u015Brednim sposobem\
  \ na pobranie bie\u017C\u0105cej daty."
title: Pobieranie aktualnej daty
weight: 29
---

## Jak to zrobić:


### Natywny PHP
Wbudowana funkcja `date()` w PHP jest najbardziej bezpośrednim sposobem na pobranie bieżącej daty. Można formatować datę na różne sposoby, określając parametr formatu.

```php
echo date("Y-m-d"); // Wyświetla: 2023-04-01 (na przykład)
echo date("l, F j, Y"); // Wyświetla: Sobota, Kwiecień 1, 2023
```

Aby uzyskać datę i czas z obsługą strefy czasowej, możesz użyć klasy `DateTime` wraz z `DateTimeZone`.

```php
$dateTime = new DateTime('now', new DateTimeZone('America/New_York'));
echo $dateTime->format('Y-m-d H:i:s'); // Wyświetla: 2023-04-01 12:00:00 (na przykład)
```

### Korzystając z Carbon (Popularna Biblioteka Stron Trzecich)
[Carbon](https://carbon.nesbot.com/) to proste rozszerzenie API dla `DateTime`, które zapewnia czyściejszy i bardziej płynny sposób pracy z datami i czasem.

Najpierw upewnij się, że masz zainstalowany Carbon za pomocą Composera:
```bash
composer require nesbot/carbon
```

Następnie możesz go użyć do pobrania bieżącej daty:

```php
use Carbon\Carbon;

echo Carbon::now(); // Wyświetla: 2023-04-01 12:00:00 (na przykład, w domyślnym formacie)
echo Carbon::now()->toDateString(); // Wyświetla: 2023-04-01
echo Carbon::now()->format('l, F j, Y'); // Wyświetla: Sobota, Kwiecień 1, 2023
```

Carbon wzbogaca obsługę dat i czasu w PHP, dodając czytelność i bogactwo funkcjonalności do manipulacji czasem, porównań i formatowania.

---
date: 2024-01-26 03:46:02.082417-07:00
description: "Hur man g\xF6r: PHP erbjuder n\xE5gra s\xE4tt att avrunda tal: `round()`,\
  \ `ceil()` och `floor()`. H\xE4r \xE4r hur de fungerar."
lastmod: '2024-03-13T22:44:37.991342-06:00'
model: gpt-4-0125-preview
summary: "PHP erbjuder n\xE5gra s\xE4tt att avrunda tal."
title: Avrundning av tal
weight: 13
---

## Hur man gör:
PHP erbjuder några sätt att avrunda tal: `round()`, `ceil()` och `floor()`. Här är hur de fungerar:

```php
echo round(3.14159);   // Returnerar 3
echo round(3.14159, 2); // Returnerar 3.14

echo ceil(3.14159);    // Returnerar 4, avrundar alltid uppåt

echo floor(3.14159);   // Returnerar 3, avrundar alltid nedåt
```

## Djupdykning
Att avrunda tal har varit väsentligt inom matematik och beräkning sedan antiken för att hantera opraktiska oändliga decimaler. I PHP kan `round()` ta en precisionparameter och läge, vilket påverkar dess beteende – `PHP_ROUND_HALF_UP`, `PHP_ROUND_HALF_DOWN`, etc., definierar hur det kommer att bete sig när det möter ett ".5"-scenario. Precision är nyckeln i finansiella applikationer där avrundning kan vara lagligt reglerad, vilket påverkar hur `round()` implementeras i koden.

Alternativ till inbyggda funktioner inkluderar anpassade avrundningsmetoder eller BC Math-funktioner för aritmetik med godtycklig precision, som är användbara för scenarier som kräver mer kontroll eller hanterar mycket stora tal där inbyggd noggrannhet kan svikta.

## Se även
Utforska mer i PHP-manualen:
- [PHP `round` funktion](https://php.net/manual/en/function.round.php)
- [PHP `ceil` funktion](https://php.net/manual/en/function.ceil.php)
- [PHP `floor` funktion](https://php.net/manual/en/function.floor.php)
- [BC Math för aritmetik med godtycklig precision](https://php.net/manual/en/book.bc.php)

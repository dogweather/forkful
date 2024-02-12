---
title:                "Avrundning av tal"
aliases: - /sv/php/rounding-numbers.md
date:                  2024-01-26T03:46:02.082417-07:00
model:                 gpt-4-0125-preview
simple_title:         "Avrundning av tal"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/rounding-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att avrunda tal innebär att klippa av decimalerna till en inställd precision, ofta till hela tal. Programmerare avrundar för att förenkla beräkningar, förbättra prestanda eller göra utdata användarvänliga.

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

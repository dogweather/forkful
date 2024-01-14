---
title:    "PHP: Skrivning till standardfel."
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför
När du skriver kod är det viktigt att ha bra felhantering för att identifiera och åtgärda eventuella problem. Att skriva till standard error är ett sätt att fånga och logga felmeddelanden i din PHP-kod.

## Så här gör du
För att skriva till standard error i din PHP-kod kan du använda funktionen `fwrite()` tillsammans med `STDERR`. Här är ett exempel som skriver ett felmeddelande till standard error:

```PHP
$fel = "Ett fel har uppstått!";
fwrite(STDERR, $fel);
```
Output:
```
"Ett fel har uppstått!"
```

## Djupdykning
Standard error är en ström som används för att skicka felmeddelanden som inte är direkt relaterade till den vanliga utdataströmmen. Till exempel kan fel i din kod skickas till standard error medan den normala utdatan används för att visa resultat eller information för användaren.

När det gäller felhantering är det också viktigt att veta att standard error är en omdirigering av utdatan för ditt PHP-skript. Det innebär att det kan skickas till en annan plats än standard utdatan, beroende på inställningarna för servern eller loggningskonfigurationen.

## Se också
- [PHP dokumentation om fwrite()](https://www.php.net/manual/en/function.fwrite.php)
- [Mer information om standard error och felhantering i PHP](https://www.w3schools.com/php/php_error.asp)
---
title:    "PHP: Läsning av kommandoradsargument"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att kunna läsa kommandoradsargument är en viktig färdighet för programmerare, särskilt i PHP. Genom att kunna läsa användarens inmatning från kommandoraden kan man skapa mer interaktiva och dynamiska program som kan anpassa sig efter användarens behov.

## Hur man gör det

Läsning av kommandoradsargument i PHP görs med hjälp av en inbyggd funktion kallad `getopt()`. Denna funktion tar emot två parametrar - en sträng med de giltiga argumenten och en array som förvarar de lästa argumenten.

```php
// Enkelt exempel
// Kör "php script.php -u username -p password" från kommandoraden
$options = getopt("u:p:");

// $options array innehåller nu:
// [
//     "u" => "username",
//     "p" => "password"
// ]
```

Det finns också möjlighet att ange en tredje parameter för att läsa in enskilda valfria argument. Dessutom kan man använda sig av `getopt()` för att läsa in olika typer av argument, som booleska och numeriska.

```php
// Exempel med flera argument, inklusive valfria och numeriska
// Kör "php script.php -u username -p password --color red -n 5" från kommandoraden
$options = getopt("u:p:", ["color:", "number:"]);

// $options array innehåller nu:
// [
//     "u" => "username",
//     "p" => "password",
//     "color" => "red",
//     "number" => 5
// ]
```

## Djupdykning

I det föregående avsnittet talade vi om att ange en sträng med de giltiga argumenten som första parameter till `getopt()`. Denna sträng kan innehålla både korta och långa argument, och man kan också ange att ett argument kräver ett värde eller inte. Till exempel:

- `u` kräver ett värde (t.ex. `username`)
- `p` kräver inte ett värde
- `color:` kräver ett värde (t.ex. `red`)
- `number::` kräver inte ett värde om man endast vill veta om argumentet finns med, men om ett värde anges så läses det in.

`getopt()` kommer också återge felmeddelanden om användaren matar in ogiltiga argument eller saknar krävda argument. Om man till exempel endast har angett strängen `u:p:`, men användaren matar in `--color red`, så kommer felmeddelandet `unknown option: color` att retuneras.

## Se även

- PHP manual om [getopt()](https://www.php.net/manual/en/function.getopt.php)
- [PHP Getopt library](https://github.com/ics-software-engineering/php-getopt) för mer komplexa fall av att läsa kommandoradsargument
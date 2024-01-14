---
title:                "PHP: Läsning av kommandoradargument"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför 
Att kunna läsa in kommandoradsargument är en praktisk och användbar färdighet för alla PHP-programmerare. Genom att kunna hantera input från kommandoraden kan du skapa dynamiska och interaktiva applikationer som kan ta emot användarinput och utföra olika funktioner baserat på det. Det är också ett sätt att automatisera uppgifter och göra dem snabbare och enklare att utföra.

## Hur man gör det 
För att läsa in kommandoradsargument i PHP, används en inbyggd funktion `getopt()`. Den här funktionen tar två argument, det första är en sträng med de olika argumenten som ska läsas in och det andra är en array som lagrar de olika argumentvärdena. Nedan finns ett exempel på hur du kan använda `getopt()`.

```PHP
$arguments = getopt("u:p:h"); 
```

I detta exempel anger vi att vi vill läsa in argumenten `u`, `p` och `h` från kommandoraden. Om vi till exempel kör vårt script med följande kommandorad:

```bash
php script.php -u admin -p password -h
```

Då kommer `$arguments` att ha följande värden:

```PHP
$arguments['u'] = 'admin';
$arguments['p'] = 'password';
$arguments['h'] = true;
```

Som du kan se så lagras varje argument som en nyckel i arrayen och värdet är det värde som angivits i kommandoraden. Om argumentet inte har ett värde (som i fallet med `h` i vårt exempel), lagras värdet som `true`.

## Djupdykning 
Förutom `getopt()` finns det andra sätt att läsa in kommandoradsargument i PHP, såsom `$_SERVER['argv']`. Det finns också många olika bibliotek och paket som erbjuder mer avancerade funktioner för att hantera kommandoradsläsning i PHP. Det är viktigt att du väljer en metod som passar bäst för ditt specifika projekt och behov.

## Se också 
- [PHP Manual - Getopt](https://www.php.net/manual/en/function.getopt.php)
- [PHP Command-line Usage](https://www.php.net/manual/en/features.commandline.usage.php)
- [GetOpt PHP Library](https://github.com/getopt-php/getopt-php)
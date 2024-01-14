---
title:    "PHP: Att skriva ut en sträng"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##Varför capitalizera en sträng?

När vi skriver kod, finns det ofta behov av att formatera text på olika sätt. En vanlig uppgift är att göra första bokstaven i en sträng till en stor bokstav, så kallad "capitalization". Detta kan vara användbart när vi vill presentera information på ett mer estetiskt sätt, eller för att möta specifika krav från användare eller system. I denna bloggpost kommer vi att utforska hur man kan göra detta med hjälp av PHP-programmering.

##Hur man kapitaliserar en sträng i PHP

För att kapitalisera en sträng i PHP, kan vi använda funktionen `ucfirst()`. Denna funktion tar en sträng som argument och returnerar samma sträng med första bokstaven kapitaliserad. Vi kan se detta i följande kodexempel:

```PHP 
$str = "hej, jag är en sträng";
echo ucfirst($str);
```

Detta kommer att ge oss resultatet "Hej, jag är en sträng". Vi kan också använda funktionen `ucwords()` som kapitaliserar första bokstaven i varje ord i en sträng. Detta kan vara användbart när vi vill ha en mer konsekvent kapitalisering i vår text.

```PHP
$str = "jag har många ord i den här strängen";
echo ucwords($str);
```

Detta kommer att ge oss resultatet "Jag Har Många Ord I Den Här Strängen". Som ni ser, kapitaliseras första bokstaven i varje ord.

##En djupdykning i kapitalisering av strängar

Det finns också andra sätt att kapitalisera en sträng i PHP, inklusive att använda inbyggda funktioner som `strtoupper()` och `strtolower()`. Dessa funktioner ändrar alla bokstäver i en sträng till antingen stora eller små bokstäver. Det kan också vara användbart att använda `mb_convert_case()` för att hantera specialtecken och teckenkodning i en sträng.

Vi kan också utveckla våra egna funktioner för att få mer kontroll över kapitaliseringsprocessen. Till exempel kan vi skapa en funktion som kapitaliserar första bokstaven av varje mening i en sträng. Detta kan uppnås genom att splitta strängen vid varje period och sedan använda `ucfirst()` på varje del av strängen.

I vissa fall kan det vara lämpligt att inte kapitalisera specifika ord i en sträng, som till exempel förkortningar eller egennamn. I sådana fall kan vi använda reguljära uttryck för att känna igen och undvika att kapitalisera dessa ord.

##Se även

Här är några länkar för att läsa mer om kapitalisering av strängar och annan användbar information om PHP-programmering:

- [PHP manual för "ucfirst" och "ucwords"](https://www.php.net/manual/en/function.ucfirst.php)
- [W3Schools tutorial om kapitalisering av strängar i PHP](https://www.w3schools.com/php/func_string_ucfirst.asp)
- [Freecodecamp tips för att hantera specialtecken vid kapitalisering i PHP](https://www.freecodecamp.org/news/php-string-upper-lower-case-functions/)
- [PHP manual för reguljära uttryck](https://www.php.net/manual/en/ref.pcre.php)
---
title:    "PHP: Omvandling en datum till en sträng"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna konvertera ett datum till en sträng är en viktig funktion inom PHP-programmering. Det gör det möjligt för användare att visa datum i ett läsbart format och använda det i olika typer av applikationer. Det är också en fördel när det gäller att sortera datum och utföra sökningar baserat på datum. 

## Så här gör du

För att konvertera ett datum till en sträng i PHP, använder du funktionen `date()`. Detta låter dig ange ett datumformat och sedan returneras datumet i den angivna strängformatet. Du kan också använda andra parametrar som låter dig manipulera datumet som du vill. Här är ett enkelt exempel:

```PHP
echo date("j F Y"); 
```

Detta kodblock kommer att returnera dagens datum i formatet "dag månad år". Resultatet kan se ut så här: 25 januari 2022. Det finns också flera andra formatalternativ som du kan använda, som "m/d/Y" för att få datumet som månad/dag/år. Du kan hitta en listning av alla tillgängliga format på PHPs officiella dokumentationssida. 

När du vill konvertera ett specifikt datum, är det bra att använda funktionen `strtotime()`. Detta låter dig ange ett datum som en sträng och sedan konvertera det till en timestamp som kan användas med `date()`-funktionen. Här är ett exempel:

```PHP
echo date("j F Y", strtotime("25 januari 2022"));
```

Detta kommer att returnera samma resultat som tidigare exempel. 

## Djupdykning

När du använder funktionen `date()` för att konvertera ett datum till en sträng, finns det också möjlighet att använda den för att manipulera datum och tider. Till exempel kan du addera eller subtrahera antal dagar, veckor, månader eller år från ett datum. Du kan också få ut enskilda element från datumet som år, månad eller timmar. Det finns en mängd olika sätt att manipulera datum med hjälp av denna funktion, så det kan vara värt att experimentera med. 

## Se även

Här är några användbara länkar till PHP-dokumentationen som du kan använda för att lära dig mer om konvertering av datum till strängar:

- [PHP date() funktion](https://www.php.net/manual/en/function.date.php)
- [PHP strtotime() funktion](https://www.php.net/manual/en/function.strtotime.php)
- [Lista över giltiga datumformat](https://www.php.net/manual/en/function.date.php)
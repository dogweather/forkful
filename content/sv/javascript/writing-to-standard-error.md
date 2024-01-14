---
title:                "Javascript: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standardfel i Javascript är en viktig del av felsökning och felhantering. Genom att utskriva felmeddelanden till standardfel kan du enkelt spåra och identifiera problem i din kod.

## Hur du gör det

För att skriva till standardfel i Javascript använder du metoden `console.error()` och skriver ut ditt felmeddelande som sträng. Exempelvis, om du vill utskriva ett felmeddelande om att en variabel inte är definierad kan du använda följande kod:

```Javascript
let namn // variabeln är inte definierad
console.error("Variabeln 'namn' är inte definierad") 
```
**Output:**
![Javascript Console Error](https://i.imgur.com/ZvDH3Ja.png)

Om du vill skriva ut flera felmeddelanden till standardfel kan du använda denna metod flera gånger:

```Javascript
console.error("Första felmeddelandet")
console.error("Andra felmeddelandet")
console.error("Tredje felmeddelandet")
```

**Output:**
```
Första felmeddelandet
Andra felmeddelandet
Tredje felmeddelandet
```

Du kan också använda `console.error()` för att utskriva information om ett kritiskt fel i en `try...catch`-block. Till exempel:

```Javascript
try {
    // kod som kan orsaka ett fel
    let name = "Emma"
    console.log(lastName) // variabeln är inte definierad
} catch (error) {
    console.error("Ett fel har uppstått:", error.message)
}
```
**Output:**
```
Ett fel har uppstått: lastName is not defined
```

## Djupdykning

Att skriva till standardfel är en bra vana att ha under utvecklingsprocessen eftersom det hjälper till att identifiera och rätta till fel snabbare. Genom att logga felmeddelanden till standardfel i produktionskoden kan du också se till att dina användare inte får en tom eller kraschad sida om ett fel skulle uppstå.

En annan fördel med att skriva till standardfel är att det kan hjälpa till att felsöka kod som körs på en klient eller server. Genom att använda `console.error()` kan du se vilken del av koden som orsakar problem och spåra det till en specifik fil och rad.

En sista sak att komma ihåg är att se till att ta bort eller kommentera ut eventuella `console.error()`-utskrifter när du är färdig med att felsöka din kod. Annars kan det leda till onödig loggning och påverka prestandan i din applikation.

## Se även

- [W3Schools - Console Methods in Javascript](https://www.w3schools.com/js/js_console.asp)
- [Mozilla Developer Network - Using the Console in the Web Console](https://developer.mozilla.org/en-US/docs/Tools/Web_Console/Using#console_methods)
- [Medium - How to Use console.error() in Javascript](https://javascript.plainenglish.io/how-to-use-console-error-in-javascript-12960b21afb3)
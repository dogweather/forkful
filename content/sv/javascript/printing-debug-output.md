---
title:                "Utskrift av felsökningsoutput"
html_title:           "Javascript: Utskrift av felsökningsoutput"
simple_title:         "Utskrift av felsökningsoutput"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför
Att skriva kod är en komplicerad process och ibland kan man behöva felsöka och hitta fel i sin kod. Genom att använda "debug output", eller utskrift av värden och variabler i koden, kan man enklare identifiera och åtgärda problem för att få en mer effektiv och korrekt kod.

## Hur man gör
Det finns flera sätt att skriva ut debug output i Javascript, men det mest vanliga är att använda metoden "console.log()". Här är ett enkelt kodexempel:

```Javascript
let num1 = 5;
let num2 = 10;
console.log(num1 + num2);
```
I detta exempel kommer värdet av variablerna(num1 och num2) att skrivas ut i konsolen, vilket i detta fall är summan av dem (15).

En annan användbar metod för att skriva ut objekt i en läsbar format är att använda "console.dir()". Här är ett exempel:

```Javascript
let person = {
  name: "Anna",
  age: 25,
  profession: "Developer"
}
console.dir(person);
```
I konsolen kommer då hela objektet att skrivas ut med dess egenskaper och värden.

## Djupdykning
Det finns också möjlighet att formatera utskriften med hjälp av s.k. placeholders. Detta gör det möjligt att skriva ut flera värden i en och samma utskrift. Här är ett exempel på hur detta kan göras:

```Javascript
let name = "Anna";
let age = 25;
console.log(`Mitt namn är ${name} och jag är ${age} år gammal.`);
```

Outputen av detta kommer att bli: "Mitt namn är Anna och jag är 25 år gammal."

Det är också möjligt att ställa in loggningsnivåer för olika typer av meddelanden. Till exempel kan man använda "console.warn()" för att skriva ut varningar och "console.error()" för att skriva ut felmeddelanden. Detta kan vara särskilt användbart när man vill ha en mer strukturerad och lättläst debug output.

## Se även
Här är några länkar till relevant information om debugging i Javascript:
- [MDN: Console](https://developer.mozilla.org/sv-SE/docs/Web/API/Console)
- [W3Schools: Debugging in Chrome](https://www.w3schools.com/js/js_debugging.asp)
- [JavaScript.info: Debugging in Chrome](https://javascript.info/testing-debugging-chrome)

Det finns också många användbara debugging-verktyg och plugins till webbläsare som kan hjälpa till att underlätta felsökningen. Utforska och hitta det som passar bäst för dig och din kod.
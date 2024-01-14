---
title:                "Javascript: Utskrift av felsökningsutdata"
simple_title:         "Utskrift av felsökningsutdata"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva ut debuggning är en viktig del av att felsöka och förstå hur ens kod fungerar. Genom att skriva utvärdet av variabler och steg i koden kan du enkelt följa dess exekvering och upptäcka eventuella fel eller buggar. Det är en enkel men effektiv praxis som kan spara dig mycket tid och frustration i slutändan.

## Hur man gör

För att skriva ut debug output i Javascript, kan du använda console.log() -funktionen. Detta skickar en sträng eller ett objekt till utmatningskonsolen, som du sedan kan se i din webbläsare eller i en separat utvecklarverktyg. Låt oss titta på ett exempel:

```Javascript
var name = "Sara";
console.log("Hello " + name + "!");
```

Output:

```Javascript
Hello Sara!
```

Som du kan se, skapade vi en variabel för namnet "Sara" och sedan skriva ut en hälsning med hjälp av den variabeln. Om du kör detta i din webbläsare eller utvecklarverktyg, kommer du att se "Hello Sara!" skrivet i konsolen. Det är ett mycket enkelt exempel, men det ger en bra översikt över hur du kan använda console.log() för att skriva ut debug output.

Utöver att bara skriva ut en sträng, kan du också använda console.log() för att skriva ut objekt som du skapar. Till exempel kan du skriva ut ett objekt och se dess egenskaper och värden. Låt oss titta på ett till exempel:

```Javascript
var car = {
    brand: "Volvo",
    model: "V60",
    year: 2015
}
console.log(car);
```

Output:

```Javascript
{ brand: "Volvo", model: "V60", year: 2015 }
```

Som du kan se, skrev vi bara ut hela objektet och det visar dess olika egenskaper och värden. Detta kan vara till stor hjälp när du behöver se vad som finns i dina objekt och hur de är strukturerade.

## Djupdykning

När det gäller att skriva ut debug output, finns det några saker att tänka på. Först och främst bör du vara försiktig med att använda console.log() för mycket i din produktionskod. För mycket utskrift kan sakta ner din applikation och göra det svårt att hitta viktig information.

Det är också viktigt att vara medveten om att vissa webbläsare inte stöder console.log(). Om du vill vara säker på att din utskrift fungerar överallt, kan du använda ett bibliotek som log4javascript eller en polyfill för console.log().

En annan bra praxis är att använda formatting tokens i din utskrift. Till exempel kan du inkludera variabler eller värden genom att använda %s eller %d tokens. Detta hjälper till att göra din utskrift mer läsbar och tydlig. Till exempel:

```Javascript
var name = "Sara";
var age = 28;
console.log("My name is %s and I am %d years old.", name, age);
```

Output:

```Javascript
My name is Sara and I am 28 years old.
```

## Se även

- [MDN: console.log()](https://developer.mozilla.org/en-US/docs/Web/API/Console/log)
- [Log4javascript](https://log4javascript.org/)
- [Console.log() Polyfill](https://github.com/paulmillr/console-polyfill)
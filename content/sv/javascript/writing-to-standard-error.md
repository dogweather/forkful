---
title:                "Att skriva till standardfel"
html_title:           "Javascript: Att skriva till standardfel"
simple_title:         "Att skriva till standardfel"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva till standard error är en vanlig praxis bland programmerare för att skicka felmeddelanden eller information om körningen av en kod till konsolen. Det är ett viktigt sätt att kommunicera med användare av en applikation eller programvara och att felsöka eventuella fel som kan uppstå.

## Hur to:

Enklaste sättet att skriva till standard error i Javascript är att använda console.error() funktionen. Här är ett exempel på hur man kan använda den:

```Javascript
console.error("Ett fel har uppstått!");
// Output: Ett fel har uppstått!
```

Funktionen accepterar även variabler och uttryck som argument:

```Javascript
let num = 5;

console.error("Variabeln num är nu: " + num);
// Output: Variabeln num är nu: 5
```

## Djupdykning:

Att skriva till standard error är inspirerat av Unix-kommandot "stderr", som används för att skicka felmeddelanden till konsolen istället för standard output ("stdout"). Det finns även andra sätt att skicka felmeddelanden, såsom att använda try-catch-block i Javascript.

Det är viktigt att använda rätt typ av utskrift beroende på informationen som ska skickas. Standard error borde endast användas för felmeddelanden och inte för vanliga meddelanden eller loggar, som bör skickas till standard output.

## Se även:

- [console.error() i MDN](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)
- [Skicka felmeddelanden med console.error i Javascript](https://www.w3schools.com/js/js_errors.asp)
- [Skillnaden mellan standard output och standard error i Unix](https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr))
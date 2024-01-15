---
title:                "Sammanslagning av strängar."
html_title:           "Gleam: Sammanslagning av strängar."
simple_title:         "Sammanslagning av strängar."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Det finns många gånger när du vill kombinera flera strängar tillsammans för att skapa en längre sträng. Det kan vara för att skapa ett meddelande, en sökväg eller en URL. Att använda tekniken att "konkatenera" strängar är ett effektivt sätt att skapa dynamiska och anpassade textsträngar.

## Så här gör du

För att konkatenera strängar i Gleam, använder du operatören `++` mellan två strängar. Låt oss titta på ett enkelt exempel:

```Gleam
let sträng1 = "Du ";
let sträng2 = "är ";
let sträng3 = "fantastisk!";
let resultat = sträng1 ++ sträng2 ++ sträng3;
```

I det här exemplet skapar vi tre olika strängar `sträng1`, `sträng2` och `sträng3`. Sedan använder vi `++` operatören för att sammanslå dem till en enda sträng som innehåller "Du är fantastisk!". Det är viktigt att notera att när du konkatenerar strängar måste du se till att de är av samma datatyp, annars kan det leda till fel.

Du kan också använda `++=` operatören för att konkatenera och tilldela strängar som detta:

```Gleam
let sträng1 = "Välkommen ";
sträng1 ++= "till min hemsida";
```

I det här fallet kommer variabeln `sträng1` att innehålla "Välkommen till min hemsida".

Du kan också konkatenera strängar med andra datatyper som `int` eller `float` genom att använda funktionen `to_string()` för att konvertera datatypen till en sträng först.

## Deep Dive

När du konkatenerar strängar i Gleam, skapas en helt ny sträng varje gång. Detta påverkar prestandan när det gäller minnesanvändning och hastighet. Därför är det viktigt att tänka på hur ofta och i vilken utsträckning du behöver använda strängkonkatering i din kod.

Du kan också använda funktionen `concat()` i standardbiblioteket `gleam/strings` för mer avancerade konkateneringsbehov. Den kan ta emot en lista av strängar och sammanslå dem till en enda sträng. Det är också mer effektivt än att använda `++` operatören när du behöver konkatenera ett stort antal strängar.

## Se även

- [Gleam dokumentation om strängar](https://gleam.run/libraries/strings)
- [Gleam standardbibliotek](https://gleam.run/libraries)
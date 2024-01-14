---
title:    "Elm: Sökning och ersättning av text"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text kan verka som en småsak, men det är faktiskt en viktig del av programmering. Genom att lära sig hur man gör detta i Elm kommer du kunna effektivisera ditt arbete och undvika onödiga fel.

## Så här gör man

För att söka och ersätta text i Elm, använd funktionen `String.replace` som tar tre argument: den sträng du vill söka igenom, det du vill ersätta och det nya ordet eller frasen du vill ersätta med. Här är ett exempel:

````Elm
myString = "Hej världen!"
result = String.replace "världen" "Elm!" myString
````

Det resulterande värdet av `result` kommer att vara "Hej Elm!".

En annan användbar funktion är `String.replaceOne`, som endast ersätter den första förekomsten av det du vill byta ut. Om du vill söka och ersätta i en lista av strängar, kan du använda `List.map` och `String.replace` tillsammans för att effektivt iterera över listan och göra de nödvändiga ändringarna.

## Djupdykning

Det finns flera andra funktioner inom Elm som kan hjälpa dig att söka och ersätta text, inklusive `String.replaceRegex` som använder reguljära uttryck för mer avancerade sök- och ersättningsmönster. Det finns också möjligheter att använda piper för att göra dessa operationer mer läsbara och mer kompakta.

## Se även

Här är några länkar till mer information om sök- och ersättningsfunktioner i Elm:

- [Officiell dokumentation för String-modulen](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Artikel om String-modulen på Elm Discovery](https://elm-discovery.org/docs/string)

Lycka till med att söka och ersätta text i dina Elm-projekt!
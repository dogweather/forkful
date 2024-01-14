---
title:    "Elm: Sammanslagning av strängar"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att sammanslå strängar (concatenating strings) är en viktig del av programmering eftersom det gör det möjligt att skapa dynamiska och anpassningsbara strängar som kan användas i olika sammanhang.

## Hur man gör det

Att sammanslå strängar i Elm är enkelt och används ofta i olika applikationer. För att sammanslå strängar i Elm använder man "+"-tecknet, vilket fungerar på samma sätt som i andra programmeringsspråk.

```Elm
let greeting = "Hej"
let name = "Anna"
let message = greeting + " " + name

message -- Resultatet blir "Hej Anna"
```

I det här exemplet används "+"-tecknet för att sammanslå "Hej" och "Anna" till en ny sträng som innehåller både hälsning och namn.

## Djupdykning

I Elm finns det också en funktion som heter "append", vilken också kan användas för att sammanslå strängar. Den här funktionen tar två strängar som argument och returnerar en ny sträng som innehåller båda.

```Elm
let first = "Elm"
let second = "programmering"
let full = String.append first second

full -- Resultatet blir "Elm programmering"
```

En annan viktig sak att komma ihåg när det gäller sammanslagning av strängar är att vara uppmärksam på vilken ordning strängarna sätts ihop i. Om man byter ordning på strängarna kan resultatet bli helt annorlunda.

## Se även

Om du vill lära dig mer om hur man sammanslår strängar i Elm, kan du kolla in följande resurser:

- Official Elm Syntax Guide: sammanslagning av strängar (https://guide.elm-lang.org/core_language.html#string-concatenation)
- Elm Programming Language Youtube Channel: Working with Strings (https://www.youtube.com/watch?v=55piJKzKZUQ)
- Elm Community - String Module (https://package.elm-lang.org/packages/elm/core/latest/String)
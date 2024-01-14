---
title:                "Elm: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

##Varför

Att söka och ersätta text i programmering kan verka som en enkel uppgift, men den kan faktiskt spara mycket tid och ansträngning. Med Elm, ett funktionellt programspråk designat för att skriva webbapplikationer, kan du enkelt söka och ersätta text i dina projekt.

## Så här gör du

För att söka och ersätta text i Elm, använd funktionen `String.replace`. Det accepterar tre argument: söksträngen, ersättningssträngen och den ursprungliga strängen. Här är ett exempel som ersätter alla förekomster av ordet "hej" med "tja" i en sträng:

```Elm
sträng = "hej världen"
nySträng = String.replace "hej" "tja" sträng
```

Outputen blir "tja världen". Som du kan se ersattes alla förekomster av "hej" med "tja".

## Djupare dykning

För mer avancerade operationer, erbjuder Elm en mäktig funktion som heter `String.replaceWith`. Den tar emot en funktion som bestämmer hur ersättningen ska göras vid varje matchning. Här är ett exempel som ersätter alla siffror i en sträng med "x":

```Elm
sträng = "123 abc 45"
nySträng = String.replaceWith (\_ -> "x") Digit sträng
```

Outputen blir "xxx abc xx". Funktionen `\_ -> "x"` betyder att ersätt varje matchning med "x".

## Se även

- Officiell dokumentation för `String.replace`: https://package.elm-lang.org/packages/elm/core/latest/String#replace
- Dokumentation för `String.replaceWith`: https://package.elm-lang.org/packages/elm/core/latest/String#replaceWith
- En handledning om hur man använder Elm för att bygga webbapplikationer: https://guide.elm-lang.org/
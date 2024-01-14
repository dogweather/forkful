---
title:                "Elm: Söka och ersätta text"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att söka och byta ut text är en nödvändig del av programmering, särskilt vid utveckling av webbapplikationer. Genom att lära sig hur man gör det effektivt kan du spara tid och undvika felaktig eller ofullständig kod.

## Hur man gör det

För att söka och byta ut text i Elm behöver man först importera "Regex" modulen. Därefter kan man använda funktionen `replace` för att söka efter en viss textsträng och ersätta den med en annan. Här är ett exempel på hur man kan använda detta i en Elm-funktion:

```Elm
import Regex exposing (replace)

replaceText : String -> String -> String -> String
replaceText old new text =
    replace (Regex.fromString old) (const new) text
```

Genom att anropa denna funktion och ange den textsträng du vill byta ut, den nya texten och den ursprungliga texten som ska ändras, kommer funktionen att returnera den ändrade texten. Till exempel, om du vill byta ut orden "Hej världen" med "Hej alla nyheter" i en textsträng, så kan du göra följande:

```Elm
replaceText "Hej världen" "Hej alla nyheter" "Hej världen, härliga nyheter!"
```

Detta kommer att returnera textsträngen "Hej alla nyheter, härliga nyheter!" som resultat.

## Djupdykning

För att förstå mer avancerad sökning och ersättning av text i Elm, kan du utforska Regex-modulen närmare. Denna modul tillåter dig att använda reguljära uttryck för att matcha textsträngar, vilket ger dig mer kontroll över vilken text som bör bytas ut och hur den ska ändras.

En annan användbar funktion för sökning och ersättning är `replaceAll`. Denna funktion fungerar på samma sätt som `replace`, men byter ut alla förekomster av den sökta texten istället för bara den första. Detta kan vara användbart om du vill göra flera ändringar i en textsträng.

## Se också

- [Elm Regex dokumentation](https://package.elm-lang.org/packages/elm/regex/latest/)
- [Reguljära uttryck för nybörjare](https://www.regexpal.com/?fam=107654)
- [Elm officiella hemsida](https://elm-lang.org/)
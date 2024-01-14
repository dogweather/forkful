---
title:                "Gleam: Att använda reguljära uttryck"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Varför använda reguljära uttryck i Gleam

Att kunna använda reguljära uttryck är ett mycket kraftfullt verktyg för att hantera textsträngar i Gleam. Med hjälp av reguljära uttryck kan du söka, extrahera och manipulera text på ett effektivt sätt. Det är ett viktigt verktyg för programmerare som vill hantera data i en strukturerad och effektivt sätt.

# Hur man använder reguljära uttryck i Gleam

För att kunna använda reguljära uttryck i Gleam behöver du först importera modulen ```gleam/regex``` och använda funktionen ```match``` för att matcha ett reguljärt uttryck mot en textsträng. Här är ett exempel på hur du kan använda reguljära uttryck för att hitta en e-postadress i en textsträng:

```Gleam
import gleam/regex

let text = "Kontakta mig på john.doe@example.com för mer information"
let regex = regex.compile("[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")

match regex with
  | Ok(matcher) ->
    case regex.find_first(text) with
      | Some(result) -> 
        let email = result.original()
        let _ = log.info("Hittade denna e-postadress: " ++ email)
      | None -> 
        let _ = log.warn("Kunde inte hitta någon e-postadress i texten.")
  | Error(_) -> 
    let _ = log.error("Ogiltigt reguljärt uttryck")
```

I exemplet ovan kompileras ett reguljärt uttryck för att söka efter e-postadresser. Sedan används funktionen ```find_first``` för att hitta den första matchningen i texten. Om det finns en matchning, loggas e-postadressen till konsolen.

# Djupdykning i användningen av reguljära uttryck

Reguljära uttryck kan verka komplicerade och svåra att förstå till en början, men när du väl lärt dig grunderna kan de vara mycket användbara. Det finns många olika funktioner som finns tillgängliga i ```gleam/regex```-modulen, såsom ```replace``` för att ersätta text och ```split``` för att dela upp en sträng baserat på ett reguljärt uttryck. Det är också möjligt att använda grupperingar i reguljära uttryck för att extrahera specifika delar av en textsträng.

Ett tips för att bli bättre på att använda reguljära uttryck är att använda ett verktyg som Regex101 för att testa och experimentera med olika uttryck. Det finns också många resurser och guider tillgängliga online för att hjälpa dig att komma igång med reguljära uttryck.

# Se även

- [Gleam dokumentation för regex modulen](https://gleam.run/packages/gleam/1.0.0/regex.html)
- [RegExr - RegEx tester och referens](https://regexr.com/)
- [Regex101 - Interaktivt verktyg för testning av reguljära uttryck](https://regex101.com/)
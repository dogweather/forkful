---
title:                "Samenvoegen van strings"
date:                  2024-01-28T21:57:04.372091-07:00
model:                 gpt-4-0125-preview
simple_title:         "Samenvoegen van strings"

category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elm/concatenating-strings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het concatenen van strings betekent het aan elkaar plakken van twee of meer stukken tekst. Het is net zo basisch en essentieel als het gebruik van ducttape, waardoor je ter plekke nieuwe strings kunt maken voor het weergeven van berichten, het creëren van sjablonen en meer.

## Hoe te:
Elm heeft een nette operator `(++)` om de dag te redden:

```Elm
greeting : String
greeting =
    "Hallo, " ++ "wereld!"

-- "Hallo, wereld!"
```

Maar soms heb je een heleboel stukken. Vrees niet, `++` is ketenbaar:

```Elm
fullName : String
fullName =
    "Elm" ++ " " ++ "Lang"

-- "Elm Lang"
```

En voor lijsten van strings is `String.join` je vriend:

```Elm
words : List String
words =
    ["Join", "the", "Elm", "club"]

sentence : String
sentence =
    String.join " " words

-- "Join the Elm club"
```

## Diepe Duik
Vroeger zou je vaak strings concatenen met complexe functies in andere talen. In Elm is het altijd een fluitje van een cent geweest dankzij de `(++)` operator. Als je echt veel aan het concatenen bent, kan efficiëntie een rol gaan spelen; het gebruik van `(++)` op lange strings kan langzamer zijn, omdat Elm elke keer door de hele string aan de linkerkant van `(++)` moet lopen.

In sommige talen bestaat ook "interpolatie", maar Elm doet niet aan stringinterpolatie. Geen zorgen echter, `(++)` en `String.join` hebben ons gedekt.

Achter de schermen, wanneer Elm concateneert, probeert het slim te zijn, vaak gebruikmakend van geoptimaliseerde JavaScript-operaties, wat uiteindelijk is waar Elm naar compileert. Dus zelfs als `(++)` eenvoudig kan lijken, is er wat slimheid gaande achter de schermen om de zaken vlot te houden.

## Zie Ook
- Elm officiële documentatie over Strings: https://package.elm-lang.org/packages/elm/core/latest/String
- Elm Handleiding, waar je meer kunt leren over strings: https://guide.elm-lang.org/strings/

---
title:    "Gleam: Extrahering av substrängar"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

I den här bloggposten kommer vi att titta på hur man kan använda sig av substrings i Gleam-programmeringsspråket. Att extrahera delsträngar kan vara användbart när man behöver manipulera textsträngar eller utföra sökningar i större textmängder.

## Så här gör man

Att extrahera substrings i Gleam är väldigt enkelt. Man kan använda sig av den inbyggda funktionen `String.slice` för att välja ut en specifik del av en textsträng.

``` Gleam
let original = "Hej, hur mår du?"
let substring = String.slice(original, 5, 8)
//substring = "hur"
```

Funktionen tar tre parametrar - den ursprungliga strängen, startpositionen och slutpositionen för den del av strängen som ska extraheras. I vårt exempel så extraherar vi en delsträng som börjar på position 5 (räknat från 0) och avslutas på position 8.

Det är också möjligt att använda negativa tal för att räkna bakifrån. Om vi vill extrahera de sista tre tecknen i vår ursprungliga sträng så skulle vi kunna använda oss av följande kod:

``` Gleam
let original = "Hej, hur mår du?"
let substring = String.slice(original, -3)
//substring = "du?"
```

Det här är bara grundläggande exempel på hur man kan använda substrings i Gleam, men det finns många fler möjligheter beroende på dina specifika behov och användningsområden.

## Deep Dive

För att gå lite djupare in i substrings i Gleam så är det viktigt att förstå hur språket hanterar teckenkodning. Gleam använder sig av Unicode för att representera tecken, vilket innebär att varje tecken kan bestå av flera bytes.

När vi använder `String.slice` så räknas positionerna utifrån antalet tecken, inte antalet bytes. Detta gör det enklare att hantera specialtecken och teckenkodning, men det är också viktigt att vara medveten om när vi arbetar med substrings.

## Se även

Här är några användbara länkar för att fortsätta utforska substrings i Gleam:

- Officiell Gleam dokumentation för `String.slice`: https://gleam.run/modules/string.html#slice
- En interaktiv tutorialsida för att lära sig grundläggande Gleam: https://gleam.run/playground/
- Ett utmärkt introduktionsinlägg om Gleam på svenska: https://dennya.com/blog/guide-to-gleam.html

Låt oss fortsätta utforska Gleam tillsammans! Ha det så roligt med substrings!
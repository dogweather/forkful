---
title:    "Elm: Översättning av titel: Omvandla en sträng till gemener"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener är en vanlig uppgift inom programmering, särskilt när man hanterar användarinmatning eller behöver göra jämförelser mellan olika strängar. Elm har inbyggda funktioner för att göra just detta, vilket gör det enkelt och effektivt att hantera gemener i sina program.

## Så här gör du

För att konvertera en sträng till gemener i Elm, kan du använda funktionen `String.toLower` som tar in en sträng som argument och returnerar en ny sträng med gemener. Här är ett exempel:

```elm
strang = "Hej Världen"
gemener = String.toLower strang

elm repl
> gemener
"hej världen"
```

Vi deklarerar en variabel `strang` med värdet "Hej Världen" och sedan använder vi funktionen `String.toLower` för att skapa en ny variabel `gemener` som innehåller den konverterade strängen. Med `elm repl` kan vi sedan testa vår kod och se att resultatet blir "hej världen".

Det är också möjligt att använda funktionen `String.toLower` på specifika delar av en sträng, istället för hela strängen. Detta gör man genom att använda funktionen `String.slice` för att välja vilka delar av strängen man vill konvertera. Här är ett exempel på detta:

```elm
strang = "Hej Världen"
förstaTre = String.slice 0 3 strang
gemener = String.toLower förstaTre

elm repl
> gemener
"hej"
```

Vi väljer första tre tecknen i vår ursprungliga sträng "Hej Världen" och konverterar enbart dessa till gemener. Resultatet blir "hej" i vår variabel `gemener`.

## Djupdykning

Under ytan använder sig funktionen `String.toLower` av en inbyggd funktion som heter `Char.toLower`. Denna funktion fungerar på ett liknande sätt men tar in enstaka tecken istället för en hel sträng. Det är denna funktion som används för att konvertera varje enskilt tecken i en sträng när vi använder `String.toLower`.

En annan intressant aspekt är att funktionen `String.toLower` endast konverterar tecken som finns definierade i Unicode-standarder. Det betyder att vissa tecken från andra språk eller symboler kanske inte konverteras. Om man vill vara säker på att konvertera alla tecken till gemener i en sträng, kan man istället använda funktionen `String.foldr`. Detta ger lite mer kontroll över processen och gör det möjligt att anpassa konverteringen efter sina specifika behov.

## Se även

- Officiell dokumentation för `String`
- Officiell dokumentation för `Char`
- Officiell dokumentation för Unicode-standarder
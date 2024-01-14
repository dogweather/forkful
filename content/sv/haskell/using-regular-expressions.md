---
title:    "Haskell: Användning av reguljära uttryck"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Regular expressions, även kallade regex, är ett mycket effektivt verktyg för att söka, matcha och manipulera textsträngar. Med hjälp av regex kan du snabbt och enkelt utföra komplexa sökningar och ersättningar i dina program. Detta gör det till ett ovärderligt verktyg för alla som arbetar med textbehandling, webbutveckling eller annan typ av datamanipulering.

## Så här gör du

För att använda regular expressions i Haskell behöver du först importera modulen `Text.Regex.Posix`. Sedan kan du använda funktionerna `=~` och `=~~` för att söka, matcha och ersätta text baserat på ett regex-uttryck.

```Haskell
-- Söka efter ett ord i en textsträng
"Detta är en textsträng" =~ "text" :: Bool -- True

-- Söka efter ett ord som matchar oavsett storlek på bokstäver
"Detta är en textsträng" =~ "TEXT" :: Bool -- True

-- Ersätta ett ord i en textsträng
subRegex (mkRegex "text") "Detta är en textsträng" "ord" -- "Detta är en ordsträng"
```

Du kan också använda speciella symboler och uttryck för att göra mer avancerade sökningar. Till exempel:

```Haskell
-- Söka efter ett eller flera tecken
"Detta är en textsträng" =~ "[a-z]+" :: Bool -- True

-- Söka efter en sifferkombination på fyra tecken
"123 Main Street" =~ "^[0-9]{4}$" :: Bool -- True

-- Matcha en e-postadress
"example@test.com" =~ "[a-zA-Z0-9_.]+@[a-zA-Z0-9_.]+" :: Bool -- True
```

Det finns många fler funktioner och möjligheter med regex i Haskell, men detta är bara ett smakprov för att komma igång.

## Djupdykning

Reguljära uttryck är ett kraftfullt verktyg, men samtidigt kan de vara komplicerade och svåra att läsa och förstå. Det är viktigt att du förstår grundläggande koncept som metakaraktärer, kvantifierare och gruppindelning om du vill utnyttja regex fullt ut.

En annan viktig del i att använda regex är att välja rätt verktyg för jobbet. I Haskell kan du välja mellan två moduler `Text.Regex.Posix` och `Text.Regex.PCRE`. De båda stödjer liknande funktioner, men PCAE-modulen är mer avancerad och har bättre stöd för Unicode.

Slutligen är det viktigt att känna till att använda regex inte är lösningen på alla problem. Ibland finns det bättre och enklare sätt att manipulera textsträngar, så se alltid till att överväga andra alternativ innan du börjar använda regex.

## Se även

Här är några resurser som kan vara till hjälp när du vill lära dig mer om regular expressions:

- [Haskell.org - Text.Regex.Posix](https://hackage.haskell.org/package/regex-posix)
- [Haskell.org - Text.Regex.PCRE](https://hackage.haskell.org/package/regex-pcre)
- [Regular-Expressions.info - Tutorial om reguljära uttryck](https://www.regular-expressions.info/tutorial.html)
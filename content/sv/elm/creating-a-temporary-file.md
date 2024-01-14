---
title:    "Elm: Skapa en tillfällig fil"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

En temporär fil är en fil som endast existerar temporärt och tas bort efter användning. Detta kan vara användbart inom programmering när man behöver skapa en fil för ett tillfälligt syfte, som till exempel att lagra data tillfälligt innan det skickas vidare till en annan plats. I denna bloggpost kommer vi att diskutera hur man skapar en temporär fil i Elm.

## Varför

Det finns flera anledningar till att man skulle vilja skapa en temporär fil i Elm. Det kan till exempel vara användbart när man behöver skicka data till en extern tjänst men vill först lagra det lokalt för att kunna återanvända det senare. Det kan också vara praktiskt när man behöver skapa en fil på en server som sedan ska raderas automatiskt när den inte behövs längre.

## Så här gör du

För att skapa en temporär fil i Elm behöver du först importera modulen "Temporary" från "elm/file". Sedan kan du använda funktionen "file" för att skapa en fil med det namn och den typ du önskar. Här är ett exempel på kod som skapar en temporär textfil med namnet "temp.txt":

```Elm
import Temporary exposing (file)

tempFile =
    file "temp.txt" "text/plain"
```

Detta skapar en temporär fil i samma mapp som din Elm-kod, och ger dig sedan en "File"-typ som du kan använda för att lägga till data till filen. Du kan också specificera en annan sökväg för filen om du vill.

När du är klar med att använda filen, kan du radera den genom att använda funktionen "delete" från modulen "Temporary". Här är en kodexempel på hur du tar bort filen "temp.txt" som vi skapade tidigare:

```Elm
import Temporary exposing (file, delete)

tempFile =
    file "temp.txt" "text/plain"

-- Resten av din kod för att lägga till data till filen

delete tempFile
```

## Gräva djupare

Det finns också flera andra funktioner i modulen "Temporary" som kan vara användbara när man arbetar med temporära filer i Elm. Till exempel kan du använda funktionen "exists" för att kontrollera om en fil redan finns innan du försöker skapa den. Det finns också funktioner för att ändra en fils namn och typ om du behöver det.

Det är också värt att notera att när du skapar en temporär fil i Elm, så skapas den faktiskt inte direkt. Istället skapas en "Task"-värde som måste köras för att faktiskt skapa filen. Detta gör det möjligt att hantera felhantering och andra saker innan filen skapas.

## Se även

- [Elm Dokumentation - Modulen Temporary](https://package.elm-lang.org/packages/elm/file/latest/Temporary)
- [Elm Dokumentation - Modulen File](https://package.elm-lang.org/packages/elm/file/latest/File)
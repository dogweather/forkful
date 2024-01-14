---
title:    "Fish Shell: Stora en sträng"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att använda sig av en "Fish Shell" för att omvandla en sträng till stora bokstäver kan vara otroligt användbart när man vill få en enhetlig presentation av text. Det kan också hjälpa till att förbättra läsbarheten och minska risken för felaktig stavning.

## Så här gör du

För att omvandla en sträng till stora bokstäver i Fish Shell, använd kommandot `string toupper`. Detta kommando tar in en sträng som argument och returnerar samma sträng med alla bokstäver omvandlade till stora bokstäver.

```Fish Shell
string toupper "hej!" # output: "HEJ!"
```

Om du vill spara resultatet av detta kommando i en variabel kan du använda dig av följande syntax:

`set STORED_STRING (string toupper "hej!")`

Nu kan du använda variabeln `STORED_STRING` för att använda den omvandlade strängen i ditt skript.

## Djupdykning

Även om det kan verka som en enkel funktion, så finns det faktiskt en hel del som händer bakom kulisserna när du omvandlar en sträng till stora bokstäver i Fish Shell.

När du kör kommandot `string toupper`, letar Fish efter din aktuella "locale" (språkkonfiguration) och använder sig av den för att avgöra vilka tecken som ska omvandlas. Varje språk har sina egna regler för vilka tecken som ska ses som små eller stora bokstäver.

En annan intressant funktion är att du kan använda `string toupper` för att omvandla textfiler på din dator. Om du kör kommandot `string toupper < test.txt`, kommer innehållet i filen "test.txt" att skrivas ut med alla bokstäver som stora bokstäver.

## Se också

- [Fish Shell dokumentation](https://fishshell.com/docs/current/index.html)
- [En guide till Fish Shell för nybörjare](https://www.makeuseof.com/tag/use-fish-shell-macos-terminal/) 
- [Fish Shell kommandon för effektivare programmering](https://opensource.com/article/20/1/fish-shell)
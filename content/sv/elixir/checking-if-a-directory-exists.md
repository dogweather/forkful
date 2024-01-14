---
title:    "Elixir: Kontrollera om en mapp finns"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp existerar kan vara en viktig del av att skriva effektiv och pålitlig kod. Det kan hjälpa till att undvika oönskade fel och störningar i ditt program, vilket i sin tur kan leda till en bättre användarupplevelse.

## Så här gör du

För att kontrollera om en mapp (eller vilken fil som helst) existerar i Elixir, kan du använda funktionen `File.exists?/1`. Den tar in en sökväg som argument och returnerar `true` om mappen existerar och `false` om den inte gör det. Låt oss titta på ett exempel:

```Elixir
iex> File.exists?("src/example_ex/dir")
true
```

I exemplet ovan kontrollerar vi om mappen "dir" existerar inuti katalogen "example_ex" i vår projektstruktur. Om mappen existerar, returnerar funktionen `true`. Annars returnerar den `false`.

## Djupdykning

När du använder `File.exists?/1` i ditt program finns det några saker att tänka på. För det första tar funktionen in en absolut sökväg som argument, så se till att specificera rätt sökväg för att undvika fel. Dessutom returnerar funktionen `true` även om filen inte är åtkomlig, så det kan vara användbart att även inkludera en kontroll för att se om filen är åtkomlig med funktionen `File.readable?/1`.

När man arbetar med filer är det också bra att ha koll på andra funktioner som kan hjälpa till att hantera dem, som `File.mkdir/1` för att skapa en ny mapp, `File.touch/1` för att skapa en ny fil och `File.rm_rf/1` för att radera en hel katalog.

## Se också

- [File Modulen](https://hexdocs.pm/elixir/File.html)
- [Tutorial: Elixir File System Basics](https://medium.com/@sascott_44494/elixir-file-system-basics-tutorial-4482edc1e312)
- [Handling Files in Elixir](https://dev.to/somitjana/handling-files-in-elixir-2oek)
---
title:                "Att skapa en tillfällig fil"
html_title:           "Bash: Att skapa en tillfällig fil"
simple_title:         "Att skapa en tillfällig fil"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skapa en tillfällig fil innebär att koda en fil som skapar sig själv för en kort tidsperiod och sedan raderar sig själv. Detta är användbart för programmerare att lagra data på skivan under en kort tid istället för att hålla all informationen i minnet.

## Hur man gör det:

```Elixir
defmodule TempFile do
    def create do
        {:ok, path} = File.mktemp()
        IO.puts("Temporary file created at #{path}")
    end
end

TempFile.create
```
När du kör ovanstående kod, skapas en tillfällig fil och dess sökväg skrivs ut.

## Djupdykning:

Att skapa tillfälliga filer i programmering har en lång historia, det började med behovet att spara diskutrymme och minnesanvändning.

Alternativt till att skapa tillfälliga filer kan programmerare också välja att implementera in-memory databaser som Redis. Dock, är användningen av dessa databaser beroende av programmets specifika krav.

I Elixir är `File.mktemp/2` standardfunktionen för att skapa tillfälliga filer. Den genererar automatiskt ett unikt filnamn och skapar filen i operativsystemets fördefinierade tempmapp.

## Se Också:

- Elixir File modul dokumentation: https://hexdocs.pm/elixir/File.html
- Guide till Redis: https://redis.io/topics/introduction
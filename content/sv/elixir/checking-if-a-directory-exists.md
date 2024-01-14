---
title:    "Elixir: Kontrollera om en mapp finns"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Undrar du någonsin om en mapp existerar i ditt program? I denna bloggpost kommer vi att titta på hur du kan kontrollera om en mapp existerar i ditt Elixir-program och varför det kan vara användbart.

## Hur man gör

För att kontrollera om en mapp existerar i ditt Elixir-program kan du använda funktionen `Path.exists?` från modulen `Path`. Här är ett exempel på hur du skulle använda den:

```Elixir
# Skapa en sökväg för den mapp du vill kolla
path = Path.join(["/hem", "användare", "bilder"])

# Kontrollera om mappen existerar
if Path.exists?(path) do
  IO.puts "Mappen finns!"
else
  IO.puts "Mappen finns inte."
end
```

I det här exemplet skapar vi först en sökväg för mappen "bilder" i användarens hemkatalog. Sedan använder vi `Path.exists?` för att kontrollera om den mappen verkligen existerar. Om den gör det, skriver vi ut "Mappen finns!", annars skriver vi ut "Mappen finns inte."

## Djupdykning

För att förstå hur `Path.exists?` fungerar, behöver vi gå lite djupare och titta på dess implementering. Funktionen använder sig av operativsystemets `File.stat/1` för att få information om en fil eller mapp. Om `File.stat/1` returnerar en lista av metadata, vet vi att mappen eller filen existerar.

Det är också värt att notera att `Path.exists?` betraktar alla typer av filer och mappar, inklusive symboliska länkar. Om du vill kontrollera om en symbolisk länk pekar till en giltig fil eller mapp kan du istället använda `File.read_link/1`.

## Se också

- [`Path.exists?` i Elixir Dokumentation](https://hexdocs.pm/elixir/Path.html#exists?/1)
- [`File.stat/1` i Elixir Dokumentation](https://hexdocs.pm/elixir/File.html#stat/1)
- [`File.read_link/1` i Elixir Dokumentation](https://hexdocs.pm/elixir/File.html#read_link/1)
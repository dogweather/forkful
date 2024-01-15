---
title:                "Att kolla om en mapp existerar"
html_title:           "Elixir: Att kolla om en mapp existerar"
simple_title:         "Att kolla om en mapp existerar"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp existerar är en viktig del av många Elixir-program. Detta kan vara användbart för att se till att nödvändiga filer finns innan de används eller för att hantera fel meddelanden om mappen inte finns.

## Hur man gör det

För att kontrollera om en mapp existerar i ett Elixir-program, kan du använda funktionen `File.exists?/1`. Detta tar en sträng som argument, som representerar sökvägen till mappen du vill kontrollera.

```elixir
if File.exists?("mapp_namn") do
  IO.puts "Mappen finns!"
else
  IO.puts "Mappen existerar inte."
end
```

I exemplet ovan kollar vi om mappen med namnet "mapp_namn" existerar. Om den gör det, kommer vi att få utskriften "Mappen finns!", annars kommer vi att få utskriften "Mappen existerar inte."

Du kan också använda `File.dir?/1` för att kontrollera om sökvägen är en mapp istället för en fil.

## Djupgående

När man kontrollerar om en mapp existerar finns det några viktiga saker att tänka på. För det första är det viktigt att tänka på vad som ska hända om mappen inte finns. Ska programmet fortsätta köra eller ska ett felmeddelande visas?

Det är också viktigt att använda rätt sökväg när du kontrollerar en mapp. Om du ger sökvägen för en undermapp till `File.exists?/1` i stället för den fullständiga sökvägen till huvudmappen, så kommer det alltid returnera `false`.

Det finns också andra funktioner som kan vara användbara när du arbetar med mappar, som `File.ls/1` för att lista alla filer och mappar i en given sökväg och `File.mkdir/1` för att skapa en ny mapp.

## Se även

- [Elixir dokumentation för File modulen](https://hexdocs.pm/elixir/File.html)
- [Guide till fil- och mapphantering i Elixir](https://www.poeticoding.com/elixirs-file-and-folder-management-functions-explained/)
- [Elixir forum tråd om att kontrollera om en mapp existerar](https://elixirforum.com/t/check-if-folder-exists/541)
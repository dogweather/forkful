---
title:                "Elixir: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kolla om en mapp existerar är en viktig del av filhantering i Elixir. Det är särskilt användbart när man bygger skalbara applikationer där filer och mappar behöver skapas eller manipuleras dynamiskt.

## Hur man gör

För att kontrollera om en mapp existerar i Elixir använder man sig av funktionen `File.dir?`. Den tar in en sökväg som argument och returnerar en boolean, `true` om mappen finns och `false` om den inte gör det.
```Elixir
path = "/Users/username/documents"
File.dir?(path)
#=> true
```

För att hantera eventuella fel och undvika krascher kan man även använda funktionen `File.stat?` som gör en liknande kontroll men returnerar `:error` om det uppstår ett fel.
```Elixir
path = "/Users/username/missing_folder"
File.stat?(path)
#=> :error
```

## Fördjupning

Det är viktigt att notera att dessa funktioner endast kontrollerar om mappen existerar, inte om den är tillgänglig eller åtkomlig. Om man vill kontrollera om man har rättigheter att läsa, skriva eller ta bort en mapp, behöver man använda andra metoder som till exempel att använda `File.readable?`, `File.writable?` eller `File.executable?`.

Det är även möjligt att använda sig av reguljära uttryckningar för att göra mer avancerade koll av sökvägar och mappar. Detta kan vara användbart om man till exempel vill kontrollera om en mapp är en undermapp till en annan mapp.

## Se även

- [Elixir Dokumentation: File.dir?](https://hexdocs.pm/elixir/File.html#dir?/1)
- [Elixir Dokumentation: File.stat?](https://hexdocs.pm/elixir/File.html#stat?/1)
- [Elixir Dokumentation: File.readable?](https://hexdocs.pm/elixir/File.html#readable?/1)
- [Elixir Dokumentation: File.writable?](https://hexdocs.pm/elixir/File.html#writable?/1)
- [Elixir Dokumentation: File.executable?](https://hexdocs.pm/elixir/File.html#executable?/1)
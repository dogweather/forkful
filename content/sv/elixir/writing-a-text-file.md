---
title:                "Skriva en textfil"
html_title:           "Arduino: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skriva en textfil innebär att spara data som text på din dator, vilket är viktigt för att kunna lagra och dela information. Programmerare gör detta för att hantera konfigurationer, loggar, eller andra uppgifter som läses av människor och program.

## Hur man gör:
```elixir
# Skapa och skriv till en textfil
File.write("hej.txt", "Hej, världen!")

# Lägg till text i en befintlig fil
File.write("hej.txt", "Hej igen!", [:append])
```
Sample Output:
```
:ok
```

## Djupdykning:
Historiskt sett har filhanteringen varit en grundläggande del av programmering och i Elixir hanteras detta smidigt via File-modulen. Alternativ till `File.write` inkluderar att streama data med `Stream` för stora filer eller använda `:io` och `:file` från Erlang när finare kontroll krävs. Implementationen använder Erlang's robusta I/O funktioner för pålitlighet och prestanda.

## Se även:
- [Elixir's officiella dokumentation](https://elixir-lang.org/docs.html) för mer om filhantering.
- [IO modulen i Elixir](https://hexdocs.pm/elixir/IO.html) för att förstå in- och ut-datamekanismer.
- [Erlang's File Modul](http://erlang.org/doc/man/file.html) för djupare insikt i den underliggande I/O funktionaliteten.
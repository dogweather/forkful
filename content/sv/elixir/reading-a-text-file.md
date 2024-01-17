---
title:                "Läsning av en textfil"
html_title:           "Elixir: Läsning av en textfil"
simple_title:         "Läsning av en textfil"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

### Vad & Varför?
Att läsa en textfil är processen att öppna en fil innehållande text och läsa innehållet i ett program. Programutvecklare gör detta för att kunna bearbeta och manipulera data som finns i en textfil, till exempel att sortera information eller utföra dataanalys.

### Hur?
Här är ett enkelt exempel på hur man kan läsa en textfil i Elixir:

```
file = File.open("min_textfil.txt")
contents = IO.read(file, :line)
IO.puts(contents)
```

```
Hej!
Välkommen till min textfil.
Här finns lite information om mig.
```

I koden ovan öppnas filen "min_textfil.txt" och sedan läses innehållet rad för rad med hjälp av ```IO.read```. Slutligen skrivs innehållet ut med ```IO.puts```.

### Deep Dive
Att kunna läsa och bearbeta textfiler är en viktig del av programmering och används för många olika ändamål. Innan moderna programmeringsspråk, som Elixir, introducerades var det vanligt att programmerare skrev sammanlagda program som endast läste och skrev till textfiler.

I Elixir kan vi även använda modulen ```File``` för att läsa innehållet i en textfil. Denna modul ger ytterligare alternativ för att läsa och skriva till filer, som t.ex. att läsa in hela innehållet på en gång istället för rad för rad.

### Se även
Här är några resurser som du kan använda när du vill lära dig mer om att läsa textfiler i Elixir:

- [Officiell Elixir dokumentation för File modulen](https://hexdocs.pm/elixir/File.html)
- [Tutorial: Working with Files in Elixir](https://www.codementor.io/@michelleminkoff/tutorial-working-with-files-in-elixir-3vwuq4m9o)
- [Elixir File IO – How to Read Write](https://qph.fs.quoracdn.net/main-qimg-646bb1ba7c383251ec8b060f5d5f0479.webp)
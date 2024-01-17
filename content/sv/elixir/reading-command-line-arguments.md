---
title:                "Att läsa kommandoradsargument"
html_title:           "Elixir: Att läsa kommandoradsargument"
simple_title:         "Att läsa kommandoradsargument"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Läsning av kommandoradsargument är en viktig del av programmering eftersom det tillåter program att ta emot användarinput från kommandoraden istället för att behöva hårdkoda indata. Det gör det möjligt för användaren att interagera med programmet och ange önskad funktionalitet, vilket bidrar till ett mer dynamiskt och flexibelt program.

## Hur du gör:

### Elixir

För att läsa kommandoradsargument i Elixir, används funktionen `System.argv/0`. Den tar inga argument och returnerar en lista med alla kommandoradsargument som skickats till programmet vid körning.

```Elixir
# Exempel: Läs och skriv ut alla kommandoradsargument
arguments = System.argv()
IO.puts(arguments)
```

Om du till exempel kör programmet med `elixir program.ex arg1 arg2`, kommer outputen att vara `[arg1, arg2]`.

## Djupdykning:

### Historisk kontext

Kommandoradsargument är en vanlig funktion i många olika programmeringsspråk och operativsystem. Konceptet finns sedan början av datorernas historia och användes för att skicka instruktioner och data till datorns operativsystem.

### Alternativ

Det finns många olika sätt att läsa kommandoradsargument i Elixir, exempelvis genom att använda bibliotek som `OptionParser` eller `exopts`. Du kan också använda en singel- eller flerradsparser, beroende på dina behov.

### Implementeringsdetaljer

Elixir använder sig av Erlang's `ode/lib/option` för att hantera kommandoradsargument. Detta gör att Elixir automatiskt hanterar argument som skickas till programmet av Erlang VM.

## Se även:

- [Officiell dokumentation för Elixir's `System.argv/0` funktion](https://hexdocs.pm/elixir/System.html#argv/0)
- [An introduction to command line arguments in Elixir](https://blog.appsignal.com/2017/07/24/code-explore-command-line-arguments-in-elixir.html) av Alexsander Akers
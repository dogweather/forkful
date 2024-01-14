---
title:                "Ruby: Skapa en temporär fil"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Att skapa en temporär fil är ett vanligt scenario i många programmeringsprojekt. Det är användbart när du behöver skriva ut temporära data eller till och med hålla tillfällig information som inte behöver sparas för framtiden. Det kan också vara användbart när du behöver skapa en fil för att sedan bearbeta den och sedan ta bort den igen.

## Hur man gör

För att skapa en temporär fil i Ruby, kan du använda metoden `Tempfile.create` och för att tilldela det till en variabel, som i följande exempel:

```Ruby
f = Tempfile.create
puts f.path
```

Detta kommer att skapa en temporär fil i det operativsystemets standardlokation. Du kan också ange en specifik mapp genom att tillägga en sökväg som argument till metoden `create`, som i följande exempel:

```Ruby
f = Tempfile.create('/tmp')
puts f.path
```

Du kan också ange ett prefix för filnamnet som genereras, vilket är användbart för att identifiera filen i en större kodbas, som i följande exempel:

```Ruby
f = Tempfile.create(['temp', '.txt'], '/tmp')
puts f.path
```

Output från alla dessa exempel är en sökväg till den skapade temporära filen, som kan användas för att skriva, läsa och hantera filinnehållet.

## Djupdykning

Metoden `Tempfile.create` tar också emot flera andra parametrar som kan vara användbara beroende på dina behov. Du kan ange ett önskat tillstånd för filen (t.ex. `'r+'` för läs- och skrivåtkomst), en särskild kodning för filen, eller en specifik filkomprimeringsnivå. Du kan läsa mer om dessa parametrar och andra funktioner i dokumentationen för `Tempfile`-klassen.

## Se också

- [Dokumentation för Tempfile-klassen](https://ruby-doc.org/stdlib-2.5.0/libdoc/tempfile/rdoc/Tempfile.html)
- [Användning av temporära filer i Ruby on Rails](https://www.rubytapas.com/2012/10/04/clean-up-after-yourself-tempfiles-vs-rails/)
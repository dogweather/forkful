---
title:                "Kontrollera om en katalog finns"
html_title:           "Elixir: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en katalog finns är en metod för att ta reda på om en viss sökväg pekar på en existerande filsystemskatalog. Programmerare gör detta för att förhindra fel vid försök till filoperationer på icke-existerande kataloger.

## Så här gör du:
I Elixir kan du använda `File.dir?/1` funktionen för att kontrollera om en katalog finns. Här är ett exempel:

```Elixir
# Kontrollerar om katalogen finns
if File.dir?("/path/to/directory")
    IO.puts "/path/to/directory finns!"
else
    IO.puts "/path/to/directory finns inte!"
end
```

Koden ovan kommer att skriva ut "/path/to/directory finns!" om katalogen finns, och "/path/to/directory finns inte!" om den inte gör det.

Surfa till [Elixir dokumentation](https://hexdocs.pm/elixir/File.html#dir%3F/1) för att se mer information.

## Utförligare förklaring
Historiskt sett har det alltid varit nödvändigt att veta om kataloger finns när man programmerar, för att undvika överraskande fel vid runtime. Elixir, liksom de flesta moderna programmeringsspråk, erbjuder därför inbyggda sätt att kontrollera detta.

Alternativa sätt att kontrollera om en katalog finns inkluderar användning av funktioner som `:file.read_file_info/1`, men det här är mer invecklat och `File.dir?/1` är att föredra i de flesta sammanhang.

`File.dir?/1` i Elixir kör egentligen `:file.read_link_info/1` funktionen i Erlang, som är det språk Elixir är byggt på. Denna funktion returnerar detaljinformation om den angivna filen (eller katalogen), inklusive om det är en katalog eller inte.

## Se även:
Det finns många resurser online för att lära sig mer om Elixir och dess inbyggda filsystemsfunktioner:

- [Elixir dokumentation](https://hexdocs.pm/elixir/File.html) – den officiella dokumentationen för Elixir's File modul.
- [Elixir Getting Started Guide](https://elixir-lang.org/getting-started/introduction.html) – En introduktion till Elixir programmering, skriven av skaparna av språket.
- [Erlang dokumentation](http://erlang.org/doc/man/file.html) - Om du är intresserad av de underliggande Erlang-funktionerna kan du kolla in den officiella Erlang-dokumentationen.
- [Elixir forum](https://elixirforum.com) - En plats för att ställa frågor, få hjälp och diskutera ideér med andra Elixir-programmerare.
---
title:                "Elixir: Läsa en textfil"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför?

Att läsa och använda textfiler är en viktig del av programmering, oavsett vilket språk du använder. I den här bloggposten kommer vi att titta närmare på hur du kan läsa en textfil med Elixir och vad du kan göra med den informationen.

## Så här gör du

För att läsa en textfil i Elixir använder vi funktionen `File.read!`. Denna funktion tar ett argument som är sökvägen till den textfil som du vill läsa. Du kan också använda `File.read` för att få tillbaka en tuple där den första delen är ett boolean som indikerar om filen lästes korrekt eller inte, och den andra delen är själva filinnehållet.

Här är ett exempel som läser en textfil med namnet "exempel.txt" och skriver ut innehållet:

```Elixir
filinnehåll = File.read!("exempel.txt")
IO.puts(filinnehåll)
```

Om din textfil är strukturerad på ett specifikt sätt, till exempel med rader av information separerade med ett visst tecken, kan du också använda funktionen `File.stream!` för att strömma filinnehållet och sedan använda Elixirs funktioner för listor och strängar för att bearbeta innehållet på ett mer avancerat sätt.

## Djupdykning

Det finns många saker du kan göra med informationen som du läser in från en textfil. Du kan manipulera datan, filtrera ut specifika delar, och till och med skriva ut den i ett nytt format. Med Elixir finns det en uppsjö av funktioner och moduler som du kan utforska och använda för att arbeta med textfiler och göra dem till en viktig del av din utveckling.

Ett annat användningsområde för att läsa från textfiler är att skapa skript som kan automatisera olika uppgifter, som till exempel att sammanställa data eller styra andra processer baserat på informationen som finns i filen.

## Se också

- [Elixir Dokumentation om `File`](https://hexdocs.pm/elixir/File.html)
- [Elixir Dokumentation om `IO`](https://hexdocs.pm/elixir/IO.html)
- [Elixir Dokumentation om listor och strängar](https://hexdocs.pm/elixir/Kernel.html#string-and-char-lists)

Tack för att du läste den här bloggposten. Tillåt dig att fortsätta utforska och ta reda på hur mycket mer du kan göra med textfiler i Elixir!
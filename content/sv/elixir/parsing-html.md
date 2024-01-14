---
title:                "Elixir: Att tolka html"
simple_title:         "Att tolka html"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Varför

Att parsa HTML är en viktig färdighet för Elixir-programmerare eftersom det är ett vanligt problem att lösa när man arbetar med webbutveckling eller webb-skrapning.

## Så här gör du

För att komma igång med att parsa HTML i Elixir, måste du först installera ett bibliotek som heter Floki. Detta bibliotek ger dig verktygen du behöver för att navigera genom HTML-dokument. Här är ett exempel på hur du kan använda det:

```Elixir
html = "<div class="article"><h1>Elixir-programmering</h1><p>En fantastiskt kraftfull språk för webbutveckling.</p></div>"

doc = Floki.parse(html)
H1 = Floki.find(doc, "h1")
p = Floki.find(doc, "p")

IO.puts "Rubrik: #{Floki.text(H1)}" #Rubrik: Elixir-programmering
IO.puts "Brödtext: #{Floki.text(p)}" #Brödtext: En fantastiskt kraftfullt språk för webbutveckling
```

Som du kan se i exemplet, parsar vi först vår HTML och lagrar den i en variabel. Sedan använder vi Floki för att hitta de specifika elementen som vi vill extrahera och skriver ut deras text med hjälp av `Floki.text()` funktionen.

## Djupdykning

Det finns många andra funktioner i Floki för att navigera genom HTML-dokument, inklusive `Floki.all()` för att hitta alla förekomster av ett visst element och `Floki.attribute()` för att hämta värden från attribut. Du kan läsa mer om dessa funktioner och mer i Flokis dokumentation.

En annan viktig aspekt av att parsa HTML i Elixir är hanteringen av fel. Om du försöker extrahera data från ett HTML-dokument som inte är välformaterat kan det leda till fel i din kod. För att hantera detta kan du använda `Floki.html_to_iodata()` för att konvertera ditt HTML till en IODATA-struktur som är säkrare att arbeta med.

## Se även

- [Floki dokumentation](https://hexdocs.pm/floki/Floki.html)
- [Elixir programing-language tutorial](https://elixir-lang.org/getting-started/introduction.html)
- [Elixir official website](https://elixir-lang.org/)
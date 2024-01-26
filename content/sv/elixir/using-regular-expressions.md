---
title:                "Använda reguljära uttryck"
html_title:           "Bash: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Regular expressions (regex) är mönster för att matcha textsträngar inom data. Programmerare använder regex för textbearbetning, dataextraktion och validering på ett kraftfullt och flexibelt sätt.

## Hur gör man:

```Elixir
# Definiera ett regex-mönster
regex = ~r/hello/

# Använda Regex.match? för att kolla om mönstret finns i en sträng
IO.puts Regex.match?(regex, "hello world") # Skriver ut: true

# Använda Regex.scan för att hitta alla förekomster
IO.inspect Regex.scan(~r/\d+/, "Det finns 12 äpplen och 55 bananer") # Utskrift: [["12"], ["55"]]
```

## Djupdykning:

Regular expressions härstammar från automatteori och formella språk, koncept som utvecklades på 1950- och 60-talen. I Elixir implementeras regex genom biblioteket `Regex`, som bygger på Erlang's `:re` modul, vilket i sin tur använder PCRE-biblioteket. Det erbjuder en hög prestanda och flexibilitet men kan vara klurigt att behärska till fullo. Alternativ till regex inkluderar strängfunktioner som `String.contains?` eller att skriva egna parser med hjälp av Elixir's makron och funktioner.

## Se även:

- Elixir's officiella dokumentation för `Regex` modulen: https://hexdocs.pm/elixir/Regex.html
- PCRE's dokumentation för att fördjupa sig i mönstersyntax: https://www.pcre.org/
- ”Learn Regex The Hard Way” för praktisk lärande av regex: https://learnregexthehardway.org/

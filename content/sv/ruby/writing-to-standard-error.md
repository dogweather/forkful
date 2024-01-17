---
title:                "Skriver till standardfel"
html_title:           "Ruby: Skriver till standardfel"
simple_title:         "Skriver till standardfel"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Skrivning till standardfel är en metod som programmerare använder för att skicka felmeddelanden till konsolen istället för den vanliga utmatningsströmmen. Detta gör det möjligt för dem att separera felmeddelandena från vanlig utdata och gör det lättare att felsöka och identifiera problem i koden.

## Så här gör du:

```Ruby
# Skriv ut ett felmeddelande till standardfel
$stderr.puts "Detta är ett felmeddelande."

# Skriv ut flera felmeddelanden till standardfel
$stderr.puts "Första felmeddelandet"
$stderr.puts "Andra felmeddelandet"

# Skriv ut ett felmeddelande med variabelvärde
name = "John"
$stderr.puts "Det finns ingen användare med namnet #{name}."
```

Exempel på output:

```
Detta är ett felmeddelande.
Första felmeddelandet
Andra felmeddelandet
Det finns ingen användare med namnet John.
```

## Fördjupning:

Historiskt sett har standardfel använts för att skriva ut felmeddelanden vid körning av kommandoradarprogram, men det används också i moderna programmeringsspråk som Ruby för att underlätta felsökning. Alternativ till att skriva till standardfel inkluderar att använda loggfiler eller att skicka felmeddelanden till en extern tjänst för övervakning. Implementationen av skrivning till standardfel varierar beroende på operativsystem, men i Ruby används motsvarande ```$stderr.puts``` som standardmetod.

## Se även:

- [Standard streams](https://en.wikipedia.org/wiki/Standard_streams)
- [Logging in Ruby](https://www.rubyguides.com/2019/01/ruby-logging/)
- [Error handling in Ruby](https://www.ruby-guides.com/2019/02/ruby-exception-handling/)
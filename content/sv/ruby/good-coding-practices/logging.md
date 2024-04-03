---
date: 2024-01-26 01:07:54.267713-07:00
description: "Hur man g\xF6r: Ruby levereras med en inbyggd modul f\xF6r loggning,\
  \ `Logger`, som \xE4r superenkel att anv\xE4nda. H\xE4r \xE4r ett snabbt exempel\
  \ f\xF6r att komma ig\xE5ng."
lastmod: '2024-03-13T22:44:38.437456-06:00'
model: gpt-4-1106-preview
summary: "Ruby levereras med en inbyggd modul f\xF6r loggning, `Logger`, som \xE4\
  r superenkel att anv\xE4nda."
title: Loggning
weight: 17
---

## Hur man gör:
Ruby levereras med en inbyggd modul för loggning, `Logger`, som är superenkel att använda. Här är ett snabbt exempel för att komma igång:

```ruby
require 'logger'

# Skapa en Logger som skriver ut till STDOUT
logger = Logger.new(STDOUT)
logger.level = Logger::INFO

# Exempel på loggmeddelanden
logger.info("Det här är ett info-meddelande")
logger.warn("Det här är ett varningsmeddelande")
logger.error("Det här är ett felmeddelande")
```

Att köra ovanstående script kommer att ge en utskrift som denna:

```
I, [2023-03-15T10:00:00.123456 #1234]  INFO -- : Det här är ett info-meddelande
W, [2023-03-15T10:00:01.234567 #1234]  WARN -- : Det här är ett varningsmeddelande
E, [2023-03-15T10:00:02.345678 #1234] ERROR -- : Det här är ett felmeddelande
```

Du kan konfigurera loggformatet och nivån för att filtrera bort onödigt brus, och du kan rikta loggarna till olika utdata, som en fil eller till och med en extern loggtjänst.

## Fördjupning
Loggning är som en uråldrig tradition inom programmering. Historiskt sett var loggar enkla textfiler, manuellt parsade med verktyg som `grep`. Men konceptet utvecklades till ett helt ekosystem av robusta loggningsramverk och tjänster som Log4j, Syslog på Linux eller Sematext och Loggly i molneran.

Rubys `Logger` är ett enkelt sätt att komma igång, men om du behöver mer kraft och flexibilitet, kanske du vill kolla in alternativ som Lograge eller Semantic Logger. Dessa bibliotek fungerar bra med Ruby-applikationer och erbjuder mer detaljerad kontroll över loggformatering, inklusive strukturerade loggar (JSON-format), bättre prestanda och sömlös integration med andra tjänster.

Varje Ruby loggbibliotek har sitt eget sätt att göra saker på, men under ytan kretsar de alla kring idén med en logger-instans som du skickar meddelanden till. Loggern hanterar dessa meddelanden baserade på inställda nivåer – DEBUG, INFO, WARN, ERROR, FATAL och UNKNOWN – och beslutar vad den ska göra med dem: skriva ut dem, spara dem i en fil, skicka dem över nätverket, osv.

## Se även
För en fördjupning i Rubys inbyggda loggningsmodul, kolla in de officiella dokumenten:

Om du är intresserad av mer avancerad loggning eller vill utforska tredjepartsgems:
- [Lograge](https://github.com/roidrage/lograge)

För allmänna loggningsmetoder och filosofi (inte specifikt för Ruby), är dessa artiklar tidlösa läsningar:
- [Googles SRE-bok - Kapitel 16: Hantering av överbelastning](https://sre.google/sre-book/handling-overload/#log-messages)
- [The 12 Factor App - Loggar](https://12factor.net/logs)

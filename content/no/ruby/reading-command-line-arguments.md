---
title:    "Ruby: Lesing av kommandolinje-argumenter"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hva er Kommandolinje Argumenter?

Kommandolinje argumenter er informasjon som kan bli gitt til et Ruby program når det blir startet fra kommandolinjen. Dette kan inneholde data som kan påvirke programmet ditt, som for eksempel filnavn til en fil du ønsker å prosessere, en spesifikk modus eller annen relevant informasjon. Å lese kommandolinje argumenter blir ofte brukt i Ruby programmer for å gi en mer fleksibel og tilpasningsdyktig opplevelse for brukeren.

## Hvordan lese kommandolinje argumenter i Ruby

I Ruby, kan vi lese kommandolinje argumenter ved hjelp av et spesialsymbol, "$" etterfulgt av et tall, som indikerer hvilket kommandolinje argument som skal leses. For eksempel, hvis du ønsker å lese det første kommandolinje argumentet, kan du bruke "$0". Andre symbolet vi kan bruke inkluderer "$*", som vil returnere en streng med alle kommandolinje argumentene. La oss se på et eksempel på hvordan dette fungerer i praksis:

```Ruby
# kode for å lese kommandolinje argumenter
puts "Det første kommandolinje argumentet er: " + $0
puts "Alle kommandolinje argumentene er: " + $*.to_s 
```

Hvis vi kjører programmet med følgende kommandolinje argumenter: "ruby programm.rb hello world", vil utgangen bli:

```
Det første kommandolinje argumentet er: hello
Alle kommandolinje argumentene er: [hello, world]
```

Som du kan se, blir kommandolinje argumentene automatisk konvertert til en liste som vi kan bruke i programmet vårt.

## Dykk dypere inn i kommandolinje argumenter

Å jobbe med kommandolinje argumenter kan være veldig nyttig i mer komplekse Ruby programmer. Noen nyttige tips å huske på er å alltid sjekke om det er nok argumenter gitt til programmet ditt, og å bruke metoden ".to_i" for å konvertere argumenter til tall. Det kan også være lurt å organisere kommandolinje argumentene i forskjellige variabler for enklere håndtering. For mer informasjon, kan du sjekke ut Ruby-dokumentasjonen om kommandolinje argumenter.

# Se også

- [Ruby Dokumentasjon - Kommandolinje Argumenter](https://ruby-doc.org/core-2.7.1/ARGV.html)
- [Ruby Programming Language - Kommandolinje Argumenter](https://www.ruby-lang.org/no/documentation/quickstart/4/)
- [The Ruby Toolbox - Kommandolinje Argumenter Librarier](https://www.ruby-toolbox.com/categories/cli_option_parsing)
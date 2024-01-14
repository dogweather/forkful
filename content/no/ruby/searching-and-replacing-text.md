---
title:                "Ruby: Søk og erstatte tekst"
simple_title:         "Søk og erstatte tekst"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Hvorfor

Å lete etter og erstatte tekst kan være en nødvendig oppgave når man jobber med Ruby programmering. Enten det er for å korrigere feil, gjøre globale endringer eller bare for å spare tid, kan å beherske denne funksjonen være svært nyttig for enhver utvikler. 

# Hvordan

For å søke og erstatte tekst i Ruby kan man bruke metoder som `gsub` og `sub`. Disse metodene tar to argumenter - en søkestreng og en erstatningsstreng. La oss se på et eksempel:

```Ruby
tekst = "Jeg elsker å programmere i Ruby!"
ny_tekst = tekst.gsub("elsker", "likker")

puts ny_tekst
# Output: "Jeg likker å programmere i Ruby!"
```

Her søker vi etter ordet "elsker" og erstatter det med "likker". Dette gjøres for hele strengen, og den endelige teksten blir skrevet ut. Legg merke til at dette endrer ikke den opprinnelige strengen `tekst`, men heller lagrer resultatet i en ny variabel `ny_tekst`.

En annen nyttig metode er `gsub!` som endrer den opprinnelige strengen direkte, i motsetning til å opprette en ny en. Dette kan være effektivt hvis du ikke trenger den originale strengen senere. La oss se på et eksempel:

```Ruby
tekst = "Hvordan går det?"
tekst.gsub!("går det", "har du det")
puts tekst
# Output: "Hvordan har du det?"
```

Vi kan også bruke Regex (regulære uttrykk) for mer avansert søk og erstatting. La oss si at vi ønsker å bytte ut alle tallene i en streng med binærtall. Dette kan gjøres som følger:

```Ruby
tekst = "123456"
tekst.gsub!(/\d+/, &:to_i.to_proc { |num| num.to_s(2) })
puts tekst
# Output: "11110001001000000"
```

Her bruker vi `\d+` for å matche alle tall i strengen og deretter omforme dem til binærtall med `to_i.to_proc`. Som du kan se, er mulighetene grenseløse når det kommer til å søke og erstatte tekst i Ruby.

# Dypdykk

Søke og erstatte tekst er en viktig del av tekstmanipulering i Ruby, men det er viktig å forstå hvordan metodene fungerer og hva som kan påvirke resultatet. For eksempel brukes `gsub` og `gsub!` for å erstatte alle forekomster av en streng, mens `sub` og `sub!` bare erstatter den første forekomsten. I tillegg kan man bruke flagg som `i` for å ignorere store og små bokstaver og `m` for å inkludere newline-karakterer.

Det er også viktig å forstå hvordan Regex fungerer, spesielt når det kommer til spesialtegn som `+`, `*` og `?`. Disse representerer ulike kvantifiseringsoperasjoner og kan føre til uventede resultater hvis de ikke brukes på riktig måte. Det er derfor viktig å lære mer om Regex og øve på å bruke det for å få maksimal nytte av søke- og erstatningsfunksjonene i Ruby.

# Se også
- [Ruby Docs: String#gsub](https://ruby-doc.org/core-2.7.2/String.html#method-i-gsub)
- [Ruby Docs: Regexp](https://ruby-doc.org/core-2.7.2/Regexp.html)
- [Learn Ruby: Regular Expressions](https://www.learnruby.org/lessons/regular-expressions)
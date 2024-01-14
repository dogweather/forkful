---
title:    "Ruby: Å konvertere en streng til små bokstaver"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

# Hvorfor

Det er en vanlig forekomst i programmering å konvertere en streng (string) til små bokstaver (lower case). Dette kan være nyttig for å sammenligne strenger eller for å sikre at brukerinndata blir behandlet på en ensartet måte.

# Hvordan man gjør det

For å konvertere en streng til små bokstaver, kan man bruke metoden `downcase` i Ruby. Denne metoden returnerer en ny streng med alle bokstavene omgjort til små bokstaver. Se eksempelet under for å se hvordan det fungerer:

```Ruby
original_streng = "HEI, DETTE ER EN STRENG"
konvertert_streng = original_streng.downcase
puts konvertert_streng
```

Dette vil gi følgende output:

```
hei, dette er en streng
```

Man kan også bruke metoden `downcase!` for å konvertere strengen direkte uten å lage en ny variabel. Merk at ved å bruke utropstegnet på slutten av metodenavnet, vil det konvertere strengen permanent og endre på den opprinnelige variabelen. Se eksempelet under:

```Ruby
original_streng = "HEI, DETTE ER EN STRENG"
original_streng.downcase!
puts original_streng
```

Dette vil gi samme output som det første eksempelet.

# Dypdykk

Det kan være nyttig å vite hvordan Ruby håndterer konvertering av strenger til små bokstaver. Ruby bruker Unicode Standard for å håndtere ulike språk og tegnsett, og derfor er det viktig å være klar over at ikke alle bokstaver vil bli konvertert til små bokstaver på samme måte. Det finnes også andre metoder for å konvertere strenger til små bokstaver basert på forskjellige språk eller for å ignorere diakritiske tegn (såkalte combining characters).

# Se også

- [Ruby dokumentasjon: String#downcase](https://ruby-doc.org/core-2.7.1/String.html#method-i-downcase)
- [Ruby dokumentasjon: String#downcase!](https://ruby-doc.org/core-2.7.1/String.html#method-i-downcase-21)
- [Ruby dokumentasjon: Unicode](https://ruby-doc.org/core-2.7.1/Encoding.html#class-String)
- [Ruby dokumentasjon: Magical Unicorn Methods](https://carol-nichols.com/2015/04/30/ruby-magical-unicorn-methods/)
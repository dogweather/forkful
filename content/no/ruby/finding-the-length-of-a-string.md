---
title:    "Ruby: Å finne lengden til en streng"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

# Hvorfor

Mange ganger i programmering, kan det være nyttig å finne lengden på en tekststreng. Dette kan være for å validere brukerinput, utføre beregninger eller rett og slett for å håndtere data på en mer effektiv måte. I denne bloggposten skal vi ta en titt på hvordan du kan finne lengden på en streng i Ruby.

# Hvordan du gjør det

I Ruby er det veldig enkelt å finne lengden på en streng. Du kan bruke `.length` eller `.size` metoden for å få lengden på en streng. La oss se på et eksempel:

```Ruby
string = "Hei, hvordan har du det?"

puts string.length # Output: 23
puts string.size # Output: 23
```

Som du ser, vil både `.length` og `.size` metoden returnere samme resultat. Det er bare to forskjellige måter å utføre samme operasjon på. Det er også viktig å merke seg at både `.length` og `.size` metoden vil også fungere på andre datatyper som strenger, for eksempel arrays og hashes.

En annen måte å få lengden på en streng er å bruke `string.count`, som vil telle antall forekomster av et spesifisert tegn eller en spesifisert gruppe av tegn i en streng. La oss se på et eksempel:

```Ruby
string = "Hei, hei, hei!"

puts string.count('hei') # Output: 3
puts string.count('i') # Output: 3
```

Her teller vi bare antall ganger ordet "hei" eller bokstaven "i" forekommer i strengen vår.

# Dykk dypere

Nå som vi har en grunnleggende forståelse av hvordan vi finner lengden på en streng i Ruby, la oss se på noen andre viktige poeng å huske på. For det første vil både `.length` og `.size` metoden returnere et heltall. Dette betyr at vi ikke kan bruke dem på en tom streng eller en streng som ikke finnes.

En annen ting å huske på er hvordan Ruby håndterer Unicode-tegn. Dette vil variere avhengig av hvilken versjon av Ruby du bruker, så det er viktig å sjekke dokumentasjonen for riktig betydning av `.length` og `.size` metoden.

# Se også

- [Ruby dokumentasjon for String Class](https://ruby-doc.org/core-2.7.2/String.html)
- [Påvisning av antall forekomster i Ruby strenger](https://www.tutorialspoint.com/ruby/ruby_strings.htm)

Takk for at du leste denne bloggposten om å finne lengden på en streng i Ruby. Vi håper det var nyttig for deg!
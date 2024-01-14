---
title:    "Ruby: Utskrift av feilsøkningsutdata"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

# Hvorfor
Når du jobber med å programmere i Ruby, vil du ofte støte på situasjoner der du trenger å spore feil eller forstå hvordan koden din fungerer. En nyttig måte å gjøre dette på er å bruke "debug output" eller "utskrift for feilsøking". Dette lar deg se verdier og variable i koden din mens den kjører, og kan hjelpe deg med å finne og løse problemer raskere.

# Hvordan
For å skrive ut feilsøkingsmeldinger i Ruby, bruker du en enkel metode kalt "puts". Denne metoden tar et argument, som kan være en streng med tekst eller en variabel. La oss se på et eksempel:

```Ruby
# Definerer en variabel
navn = "Marius"

# Skriver ut en streng og variabelt ved hjelp av puts
puts "Hei, mitt navn er #{navn}"
```

Dette vil gi følgende output når koden kjører:

```Ruby
Hei, mitt navn er Marius
```

Som du kan se, blir verdien av variabelen (i dette tilfellet "Marius") satt inn i strengen når den skrives ut. Dette kan være nyttig når du vil se hva som skjer med en variabel i løpet av koden din.

# Dypdykk
En annen måte å bruke "puts" på er å skrive ut hele objekter eller variable. Dette kan være spesielt nyttig når du jobber med komplekse datastrukturer. For å gjøre dette, bruker du bare metoden "inspect" på objektet eller variabelen din, som vist i eksempelet nedenfor:

```Ruby
# Definerer en hash med navn og alder
person = {navn: "Marius", alder: 30}

# Skriver ut hashen med "inspect" metoden
puts person.inspect
```

Dette vil gi følgende output:

```Ruby
{:navn => "Marius", :alder => 30}
```

Som du kan se, blir hele hashen skrevet ut med nøkkelen og verdien for hver element på en enkel måte. Dette kan bidra til å identifisere problemer med datastrukturene dine og feil i koden.

# Se også
- [Ruby Dokumentasjon for "puts" metoden](https://ruby-doc.org/core-2.7.0/Kernel.html#method-i-puts)
- [En omfattende guide til feilsøking i Ruby](https://www.educative.io/blog/ruby-debugging-complete-tutorial)
- [En YouTube-video om debugging i Ruby av Derek Banas](https://www.youtube.com/watch?v=5VfRZQsiB5w)
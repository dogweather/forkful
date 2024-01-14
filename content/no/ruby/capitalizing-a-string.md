---
title:    "Ruby: Stor bokstavføring av en streng"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor

Har du noen gang lurt på hvorfor noen programmerere velger å bruke tid på å stille inn bokstaver i en streng til enten store eller små bokstaver? Eller kanskje du har sett en fremmed kode med "capitalize" funksjon og lurer på hva formålet er? I denne artikkelen vil vi ta en titt på hvorfor og hvordan å kapitalisere en streng i Ruby.

# Hvordan

For å kapitalisere en streng i Ruby er det flere forskjellige metoder å velge mellom, avhengig av dine behov. La oss se på noen eksempler på kode og resultat.

```Ruby
str = "velkommen til ruby-programmering"

puts str.capitalize
```

Dette vil skrive ut "Velkommen til ruby-programmering". Her bruker vi den innebygde funksjonen `capitalize` som gjør den første bokstaven i strengen til en stor bokstav og resten til små bokstaver.

```Ruby
puts str.upcase
```

Dette vil skrive ut "VELKOMMEN TIL RUBY-PROGRAMMERING". Her bruker vi funksjonen `upcase` for å gjøre alle bokstaver i strengen til store bokstaver.

Det er også en `downcase` funksjon som gjør alle bokstaver til små bokstaver, og en `swapcase` funksjon som bytter mellom store og små bokstaver.

# Dypdykk

Nå som vi har sett på hvordan man kan kapitalisere en streng i Ruby, la oss dykke litt dypere inn i hvorfor man ville gjøre dette. En vanlig grunn til å kapitalisere en streng er for å gjøre den mer leselig for brukeren. For eksempel i en melding eller en tittel. Det kan også brukes til å standardisere en visning av data i et program.

# Se også

For mer informasjon om strenger og metoder i Ruby, ta en titt på disse ressursene:

- [Ruby offisiell dokumentasjon](https://ruby-lang.org/en/documentation/)
- [Why's (poignant) Guide to Ruby](https://mislav.net/rails/writing-ruby-code/)
- [Ruby Tutorials på YouTube](https://www.youtube.com/results?search_query=ruby+tutorials)
---
title:    "Ruby: Skriving av tester"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor
Velkommen til min nye bloggpost om testing i Ruby-programmering! Kanskje du lurer på hvorfor det er viktig å skrive tester når man koder. Vel, det er fordi tester hjelper oss å sikre at koden vår fungerer som den skal og forhindrer feil og bugs i å havne i produksjon. Det er en viktig del av å lage kvalitetskoder og sikre at prosjektene våre fungerer som de skal.

## Hvordan
Nå som du vet hvorfor vi skriver tester, la oss se på hvordan vi kan gjøre det i Ruby. Det første trinnet er å opprette en ny fil for testene våre, gjerne med navnet "test.rb". La oss starte med et enkelt eksempel på en test for en metode som skal legge sammen to tall:

```Ruby
def add(x, y)
  return x + y
end

puts "Test 1:"
puts "Forventet output: 5"
puts "Faktisk output: #{ add(2, 3) }"
```

I testen har vi opprettet en metode som heter "add" som tar inn to tall og returnerer summen av dem. Deretter bruker vi "puts" for å skrive ut forventet output og faktisk output ved å kalle på metoden vår og legge inn argumentene 2 og 3. Når du kjører denne testen, bør du få "Test 1:" som output, etterfulgt av forventet og faktisk output.

## Dypdykk
Nå som vi har et enkelt eksempel, la oss gå litt dypere inn i testing i Ruby. Det er mange forskjellige test-rammeverk å velge mellom, men de to mest populære er RSpec og MiniTest. Begge har sine fordeler og ulemper, så det kan være lurt å prøve begge og se hva som fungerer best for deg og prosjektet ditt.

En annen viktig del av testing er å lære om ulike typer tester, som f.eks. enhetstesting, integrasjonstesting og akseptansetesting. Det kan være lurt å lese mer om disse og forstå når det er hensiktsmessig å bruke dem.

Det er også viktig å skrive "clean code" når man skriver tester, med gode navn på tester og beskrivende meldinger som forklarer hva testene sjekker. Dette gjør det lettere å feilsøke og forstå resultatene av testene.

## Se også
Her er noen lenker til videre lesning om testing i Ruby:

- [RSpec: The Ruby Testing Framework](https://rspec.info/)
- [MiniTest: A Low-impact Testing Framework](https://github.com/seattlerb/minitest)
- [Types of Tests in Ruby](https://www.jstorimer.com/blogs/workingwithcode/7766103-what-are-the-types-of-test-in-ruby)
- [Clean Code: Writing Tests](https://cleancoders.com/episode/clean-code-episode-7/show)
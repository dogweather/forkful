---
title:                "Ruby: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvis du noen gang har sittet fast i en bug i koden din, så vet du hvor frustrerende det kan være. En måte å finne og fikse disse bugene på er å bruke "debug output" eller å skrive ut informasjon mens koden kjører. Dette gir deg mulighet til å se hva som skjer i koden din og hvorfor den ikke fungerer som den skal. I denne bloggposten vil vi utforske hvordan man kan bruke debug output i Ruby-programmering.

# Hvordan

For å legge til debug output i koden din, kan du bruke "puts" metoden i Ruby. Denne metoden viser en melding eller verdi til terminalen eller konsollen når programmet kjører. La oss for eksempel si at du har en variabel som heter "navn" og du vil se hva den inneholder når koden kjører. Du kan legge til denne linjen med debug output:

```Ruby
puts navn
```

Når du kjører koden din, vil du se verdien av variabelen "navn" i terminalen. Dette kan hjelpe deg å identifisere eventuelle feil eller uønskede verdier som kan føre til en bug.

Du kan også bruke "p" metoden for å skrive ut mer komplekse objekter som hash eller array. Denne metoden viser hele objektet og dens innhold, som kan være nyttig når du feilsøker.

```Ruby
p array
```

# Dypdykk

Når du bruker debug output er det viktig å være forsiktig med hvor mye du skriver ut. Hvis du skriver ut for mye informasjon, kan det bli forvirrende og vanskelig å finne ut hva som er relevant. Du kan også bruke "if" betingelser for å bestemme når debug output skal vises. Dette kan hjelpe deg med å begrense mengden informasjon som blir skrevet ut og gjøre det enklere å finne feilen.

Det kan også være lurt å bruke kommentarer sammen med debug output for å gjøre det tydelig hva som blir skrevet ut og hvorfor. Dette kan hjelpe deg og andre utviklere med å forstå koden bedre og raskere finne feilen.

# Se også

- [Ruby Dokumentasjon: Debugging](https://www.ruby-lang.org/no/documentation/debugging/)
- [Ruby on Rails Guides: Debugging](https://guides.rubyonrails.org/v4.2.4/debugging_rails_applications.html)
- [RailsTapas: Debugging Techniques in Ruby on Rails](http://railscasts.com/episodes/54-debugging-techniques?view=comments)
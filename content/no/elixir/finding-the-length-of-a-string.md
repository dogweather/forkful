---
title:                "Å finne lengden av en tekststreng"
html_title:           "Elixir: Å finne lengden av en tekststreng"
simple_title:         "Å finne lengden av en tekststreng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?

Å finne lengden på en streng er en måte for programmerere å bestemme hvor mange tegn en spesifikk streng inneholder. Dette er nyttig for å håndtere tekst og data i koden vår.

Slik Gjør Du:
```elixir
# Her er noen eksempler på hvordan du kan finne lengden på en streng i Elixir:

# Metode 1: Bruk String.length funksjonen
string = "Hei, dette er en streng!"
len = String.length(string)
IO.puts(len) #output: 25

# Metode 2: Itererer gjennom strengen og teller antall tegn
def count(string) do
    count = 0
    for char <- string, do: count = count + 1
    count
end
len = count("Hei, dette er en streng!")
IO.puts(len) #output: 25
```

Deep Dive:

Å finne lengden på en streng er en av de mest grunnleggende funksjonene i programmering, og det er en viktig del av datatyper og hvordan vi håndterer tekst og data. Denne funksjonen kan også brukes til å validere input, for eksempel for å sikre at en brukers input ikke overstiger en bestemt lengde. Alternativt kan man også bruke regex for å finne lengden på en streng, men dette kan være mer komplekst og mindre effektivt.

Se også:
- Elixir Dokumentasjon: https://hexdocs.pm/elixir/String.html#length/1
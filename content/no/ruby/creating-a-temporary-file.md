---
title:                "Oppretting av en midlertidig fil"
html_title:           "Ruby: Oppretting av en midlertidig fil"
simple_title:         "Oppretting av en midlertidig fil"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Å lage midlertidige filer er en nyttig måte å håndtere data på når du jobber med programmering. Disse filene kan inneholde midlertidige eller sensitive data som bare trengs for en kort periode, og skaperen av filen kan være sikker på at den vil bli slettet når det er nødvendig.

## Hvordan du lager en midlertidig fil i Ruby
```Ruby
# Først importerer du nødvendige biblioteker
require 'tempfile'

# Deretter kan du opprette en midlertidig fil som følger:
tmp_file = Tempfile.new('example')

# Du kan nå skrive til filen ved å bruke "puts"-metoden:
tmp_file.puts "Dette er en midlertidig fil som inneholder noen data."

# Du kan også lese data fra filen:
puts tmp_file.read # Vil skrive ut "Dette er en midlertidig fil som inneholder noen data."

# Når du er ferdig med å bruke filen, må du huske å slette den:
tmp_file.close
tmp_file.unlink # Vil slette filen fra systemet

# Du kan også angi en spesifikk mappe for filen å bli opprettet i:
tmp_file = Tempfile.new('example', '/path/to/directory')
```

## Dypdykk
Ruby's `Tempfile`-modul gjør det enkelt å opprette og manipulere midlertidige filer. Du kan bruke `new`-metoden for å opprette filen og spesifisere en prefiks for filnavnet. Du kan også spesifisere en bestemt mappe for filen å bli opprettet i ved å legge til en parameter for banen.

Det er også verdt å merke seg at når `Tempfile`-objektet er initialisert, vil filen automatisk bli slettet når den ikke lenger blir brukt. Du kan også bruke `unlink`-metoden for å slette filen manuelt. En annen nyttig funksjon er at `Tempfile`-objektet vil følge sikkerhetsreglene for midlertidige filer, noe som betyr at filen vil bli automatisk slettet på en sikker måte som forhindrer uautorisert tilgang til dataene i filen.

## Se også
- [Ruby Tempfile dokumentasjon](https://ruby-doc.org/stdlib-2.6.3/libdoc/tempfile/rdoc/Tempfile.html)
- [Effektiv håndtering av midlertidige filer i Ruby](https://www.rubyguides.com/2018/08/ruby-tempfile/)
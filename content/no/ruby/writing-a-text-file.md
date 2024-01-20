---
title:                "Skriving av en tekstfil"
html_title:           "Arduino: Skriving av en tekstfil"
simple_title:         "Skriving av en tekstfil"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skriving til tekstfil er lagring av data til en fil på disk. Programmerere gjør dette for å persistere informasjon mellom økter eller for å dele data med andre programmer.

## Hvordan:
```Ruby
# Åpne en fil og skriv til den
File.open("eksempel.txt", "w") do |fil|
  fil.puts("Hallo Norge!")
end

# Legge til i en eksisterende fil
File.open("eksempel.txt", "a") do |fil|
  fil.puts("Velkommen til Ruby-programmering.")
end
```

Sample Output:
```
Hallo Norge!
Velkommen til Ruby-programmering.
```

## Dypdykk:
I tidlige datamaskiner skrev man data til magnetbånd, men med tiden flyttet dette til harddisker og andre lagringsmedier. Alternativer til Ruby sin filskrivningsmetode inkluderer bruk av databasesystemer eller skytjenester for større skalerbarhet. Implementeringsdetaljer i Ruby håndteres maskinent har `IO` klasse som underbygger filoperasjoner, noe som gir abstraksjon og enkel bruk.

## Se også:
- Ruby's IO-dokumentasjon: https://ruby-doc.org/core-3.0.0/IO.html
- Ruby Quickstart Guide: https://www.ruby-lang.org/en/documentation/quickstart/
- Filhåndtering og persistens: https://www.rubyguides.com/2015/05/working-with-files-ruby/
---
title:                "Sjekke om en mappe eksisterer"
html_title:           "Ruby: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sjekke om en mappe eksisterer er en viktig del av programmering, spesielt når det gjelder å håndtere filer. Det lar deg vite om du kan kjøre operasjoner som å opprette nye filer eller lese eksisterende filer.

## Hvordan

Det er enkelt å sjekke om en mappe eksisterer i Ruby ved hjelp av `Dir.exist?` metoden. Her er et eksempel på hvordan det kan gjøres:

```Ruby
if Dir.exist?("mappe_navn")
  puts "Mappen finnes!"
else
  puts "Mappen finnes ikke."
end
```

Dette vil skrive ut henholdsvis "Mappen finnes!" eller "Mappen finnes ikke." avhengig av om mappen eksisterer eller ikke.

## Dykke dypere

I tillegg til `Dir.exist?` metoden, er det også mulig å bruke `File.directory?` metoden for å sjekke om en mappe eksisterer. Forskjellen er at `Dir.exist?` vil returnere `true` for både mapper og filer, mens `File.directory?` bare vil returnere `true` for mapper.

En annen viktig ting å huske på er at disse metodene bare sjekker om en mappe eller fil eksisterer på det nåværende tidspunktet. Det er mulig at en mappe kan bli opprettet eller slettet mens programmet kjører, så det kan være lurt å sjekke på nytt hvis programmet ditt utfører operasjoner som involverer mappen.

## Se også

- [Ruby dokumentasjon for Dir](https://ruby-doc.org/core-#{RUBY_VERSION}/Dir.html)
- [Ruby dokumentasjon for File](https://ruby-doc.org/core-#{RUBY_VERSION}/File.html)
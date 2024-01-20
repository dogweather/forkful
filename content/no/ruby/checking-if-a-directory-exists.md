---
title:                "Sjekker om en mappe eksisterer"
html_title:           "Lua: Sjekker om en mappe eksisterer"
simple_title:         "Sjekker om en mappe eksisterer"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?

Sjekking av om en mappe eksisterer er prosessen hvor programmeret fastslår om det er en gitt mappe i systemet eller ikke. Dette er nyttig for å unngå feil som kan forekomme når programmer prøver å få tilgang til mapper som ikke eksisterer.

## Hvordan Gjøre Det:

Eksemplet nedenfor viser hvordan sjekke om en katalog eksisterer i Ruby. 

```Ruby
def directory_exists?(directory)
  Dir.exist?(directory)
end

puts directory_exists?('/path/to/directory') # returnerer enten true or false
```

Ved å kjøre koden ovenfor, vil du se `true` hvis mappen '/path/to/directory' eksisterer, og `false` hvis den ikke gjør det.

## Dyp Dykk:

Den `Dir.exist?` metoden har blitt brukt til å sjekke om en mappe eksisterer siden Ruby 1.9.3. Før denne versjonen, måtte programmerere bruke `File.directory?` metoden.

Et alternativ til `Dir.exist?` er `File.directory?`, som også returnerer `true` hvis katalogen eksisterer. Bruken er nesten lik:

```Ruby
def directory_exists?(directory)
  File.directory?(directory)
end

puts directory_exists?('/path/to/directory') # returnerer enten true or false
```

Forskjellen mellom `Dir.exist?` og `File.directory?` er at sistnevnte også sjekker om stien peker på et filsystem objekt som er en mappe, mens `Dir.exist?` kun sjekker om stien er gyldig.

## Se Også:

For mer komplette diskusjoner og relaterte ressurser om dette emnet, se følgende lenker:

1. Ruby Dokumentasjon: [Dir.exist?](https://ruby-doc.org/core-2.6.3/Dir.html#method-c-exist-3F)
2. Ruby Dokumentasjon: [File.directory?](https://ruby-doc.org/core-2.6.3/File.html#method-c-directory-3F)
3. Stack Overflow: [Hvordan sjekke om en katalog eksisterer i Ruby](https://stackoverflow.com/questions/2108727/which-in-ruby-checking-if-a-file-exists)
4. Ruby Forum: [Diskusjoner om katalog behandling i Ruby](https://www.ruby-forum.com/c/ruby)
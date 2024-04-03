---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:15.859130-07:00
description: "\xC5 sjekke om en mappe eksisterer i Ruby gj\xF8r det mulig for programmerere\
  \ \xE5 bekrefte tilstedev\xE6relsen av en mappe f\xF8r de utf\xF8rer operasjoner\
  \ som \xE5 lese\u2026"
lastmod: '2024-03-13T22:44:41.347608-06:00'
model: gpt-4-0125-preview
summary: "\xC5 sjekke om en mappe eksisterer i Ruby gj\xF8r det mulig for programmerere\
  \ \xE5 bekrefte tilstedev\xE6relsen av en mappe f\xF8r de utf\xF8rer operasjoner\
  \ som \xE5 lese filer eller opprette nye mapper."
title: Sjekker om en mappe eksisterer
weight: 20
---

## Hvordan:
Rubys standardbibliotek tilbyr enkle metoder for å sjekke en mappe sitt eksistens. Slik gjør du det med ren Ruby, uten å trenge noen tredjeparts biblioteker:

```ruby
require 'fileutils'

# Sjekk om en mappe eksisterer
if Dir.exist?('/sti/til/mappe')
  puts 'Mappen eksisterer.'
else
  puts 'Mappen eksisterer ikke.'
end
```
Eksempel på utskrift:
```
Mappen eksisterer.
```
Eller:
```
Mappen eksisterer ikke.
```

I tillegg til å bruke `Dir.exist?`, kan du også benytte deg av `File.directory?`-metoden som returnerer `true` hvis den gitte stien er en mappe:

```ruby
if File.directory?('/sti/til/mappe')
  puts 'Mappen eksisterer.'
else
  puts 'Mappen eksisterer ikke.'
end
```
Både `Dir.exist?` og `File.directory?` er en del av Rubys standardbibliotek og krever ikke noen eksterne gems for å bruke, noe som gjør dem til praktiske og effektive alternativer for mappesjekker.

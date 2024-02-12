---
title:                "Sjekker om en mappe eksisterer"
date:                  2024-02-03T19:08:15.859130-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sjekker om en mappe eksisterer"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sjekke om en mappe eksisterer i Ruby gjør det mulig for programmerere å bekrefte tilstedeværelsen av en mappe før de utfører operasjoner som å lese filer eller opprette nye mapper. Dette er avgjørende for å unngå feil i filhåndtering og sikre påliteligheten av filsystemmanipulasjoner.

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

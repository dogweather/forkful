---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:59.880838-07:00
description: "Hvordan: Ruby gj\xF8r filoperasjoner enkle. For \xE5 skrive til en fil,\
  \ kan du bruke Rubys innebygde `File`-klasse. F\xF8lgende eksempel demonstrerer\
  \ hvordan du\u2026"
lastmod: '2024-03-13T22:44:41.351778-06:00'
model: gpt-4-0125-preview
summary: "Ruby gj\xF8r filoperasjoner enkle."
title: Skrive en tekstfil
weight: 24
---

## Hvordan:
Ruby gjør filoperasjoner enkle. For å skrive til en fil, kan du bruke Rubys innebygde `File`-klasse. Følgende eksempel demonstrerer hvordan du åpner en fil for skriving (`"w"`-modus) og tillegging (`"a"`-modus), deretter skrive en streng til den, og sikre at filen lukkes etterpå:

```ruby
# Skriver nytt innhold til en fil, overskriver eksisterende innhold
File.open("example.txt", "w") do |file|
  file.puts "Hallo, Ruby!"
end

# Legger til innhold på slutten av en fil
File.open("example.txt", "a") do |file|
  file.puts "Legger til en annen linje."
end
```
Etter å ha kjørt begge kodestykkene, vil innholdet i `example.txt` være:
```
Hallo, Ruby!
Legger til en annen linje.
```

### Bruk av et tredjepartsbibliotek: FileUtils
For mer komplekse filoperasjoner kan Ruby standardbibliotek `FileUtils` være praktisk, selv om for grunnleggende filskriving er standard `File`-metoder tilstrekkelige. Men, hvis du ønsker å kopiere, flytte, fjerne eller utføre andre filsystemoperasjoner i forbindelse med filskriving, er `FileUtils` verdt å utforske.

Et eksempel på bruk av `FileUtils` for å opprette en mappe og deretter skrive til en fil inni den mappen:
```ruby
require 'fileutils'

FileUtils.mkdir_p 'logs'
File.open("logs/today.log", "w") do |file|
  file.puts "Logginnlegg: #{Time.now}"
end
```

Dette demonstrerer oppretting av en ny mappe `logs` hvis den ikke allerede eksisterer, og skriving til en ny fil `today.log` inni den, og viser både mappe- og filmanipulasjon uten å direkte skrive med FileUtils, men ved å bruke dens håndtering av mapper.

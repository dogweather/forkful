---
aliases:
- /no/ruby/writing-a-text-file/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:59.880838-07:00
description: "\xC5 skrive til en tekstfil i Ruby er en grunnleggende operasjon som\
  \ lar deg lagre utdata og data p\xE5 en vedvarende m\xE5te, slik at data kan tilg\xE5\
  s eller endres\u2026"
lastmod: 2024-02-18 23:08:54.456052
model: gpt-4-0125-preview
summary: "\xC5 skrive til en tekstfil i Ruby er en grunnleggende operasjon som lar\
  \ deg lagre utdata og data p\xE5 en vedvarende m\xE5te, slik at data kan tilg\xE5\
  s eller endres\u2026"
title: Skrive en tekstfil
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive til en tekstfil i Ruby er en grunnleggende operasjon som lar deg lagre utdata og data på en vedvarende måte, slik at data kan tilgås eller endres senere. Programmerere utfører ofte denne oppgaven av grunner som logging, lagring av konfigurasjoner eller eksport av data i et menneskelesbart format.

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

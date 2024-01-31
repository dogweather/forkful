---
title:                "Sjekke om en mappe eksisterer"
date:                  2024-01-20T14:58:34.290536-07:00
html_title:           "Fish Shell: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"

category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sjekke om en mappe finnes i Ruby betyr å verifisere at en bestemt sti leder til en faktisk mappe i filsystemet. Programmerere gjør dette for å forhindre feil før de prøver å lese fra eller skrive til filer i mappen.

## Hvordan gjøre det:
```Ruby
require 'fileutils'

# Sjekk om en mappe finnes
if Dir.exist?('/path/to/directory')
  puts 'Mappen finnes!'
else
  puts 'Mappen finnes ikke.'
end

# Eksempel output:
# Mappen finnes!
# eller
# Mappen finnes ikke.
```

## Dypdykk
Å sjekke om en mappe finnes er kjernen i mange Ruby-skript som jobber med filsystemet. I Ruby's tidlige dager brukte vi `File.directory?('/path/to/directory')` for å oppnå dette. `Dir.exist?` og `File.directory?` er i bunn og grunn det samme i moderne Ruby-versjoner, men `Dir.exist?` er mer intuitiv når det kommer til mappenavngivning. Når en mappe ikke finnes, og man prøver å lese/skrive til den, vil Ruby kaste en `Errno::ENOENT` (Error NO ENTry) unntak - noe man absolutt vil unngå.

Det finnes alternativer som `FileUtils.cd('/path/to/directory', Verbose: false)` som kaster en unntak hvis mappen ikke finnes, men dette endrer også den nåværende arbeidsmappen i prosessen, noe som kanskje ikke er ønskelig.

Detaljert, `Dir.exist?` kaller `File.stat(path).directory?` under hetten, og det sjekker filsystemet på lavt nivå for å bekrefte at stien leder til en mappe.

## Se også
- Ruby-dokumentasjonen for [Dir.exist?](https://ruby-doc.org/core-3.1.2/Dir.html#method-c-exist-3F)
- Ruby-dokumentasjonen for [File.directory?](https://ruby-doc.org/core-3.1.2/File.html#method-c-directory-3F)
- Stack Overflow-diskusjoner om å håndtere filer og mapper i Ruby
- `FileUtils` modulen for mer avansert filhåndtering [FileUtils](https://ruby-doc.org/stdlib-3.1.2/libdoc/fileutils/rdoc/FileUtils.html)

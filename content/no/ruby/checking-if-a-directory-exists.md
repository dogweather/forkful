---
title:                "Ruby: Ved å sjekke om en mappe eksisterer"
simple_title:         "Ved å sjekke om en mappe eksisterer"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor?
Det å sjekke om en mappe eksisterer kan være en viktig del av å programmere i Ruby. Dette er spesielt nyttig hvis du skal lage et program som skal håndtere filsystemet på datamaskinen din.

## Hvordan gjøre det?
Det er enkelt å sjekke om en mappe eksisterer ved å bruke Ruby kode. Først må du inkludere "fileutils" biblioteket i koden din ved hjelp av require statement:

```Ruby
require 'fileutils'
```

Deretter kan du bruke File.exist? metoden og gi den stien til ønsket mappe som et argument. Hvis mappen eksisterer, vil metoden returnere true, hvis ikke vil den returnere false. For eksempel:

```Ruby
if File.exist?('mappe_navn')
  puts "Mappen eksisterer."
else
  puts "Mappen eksisterer ikke."
end
```

Dette vil gi følgende output hvis mappen eksisterer:

```Ruby
Mappen eksisterer.
```

Hvis mappen ikke eksisterer, vil output være:

```Ruby
Mappen eksisterer ikke.
```

## Dypdykk
I tillegg til File.exist? metoden, finnes det også andre metoder for å sjekke om en mappe eksisterer. Du kan for eksempel bruke Dir.exist? metoden for å sjekke om en mappe eksisterer, men denne metoden vil returnere false hvis stien som blir gitt er en fil, og ikke en mappe.

Det finnes også flere måter å håndtere hva som skjer hvis mappen ikke eksisterer. Du kan for eksempel bruke File.directory? metoden for å sjekke om en mappe er en faktisk mappe, og ikke en fil. Du kan også bruke Dir.glob metoden for å søke gjennom alle mapper på en gitt sti, og se om den eksisterer der.

## Se også
- [fileutils biblioteket i Ruby](https://ruby-doc.org/stdlib-2.6.3/libdoc/fileutils/rdoc/FileUtils.html)
- [Sjekke om en fil eksisterer i Ruby](https://www.digitalocean.com/community/tutorials/how-to-check-if-a-file-or-directory-exists-in-ruby)
- [Andre metoder for å sjekke filsystemet i Ruby](https://www.rubyguides.com/2017/09/ruby-file-exists/)
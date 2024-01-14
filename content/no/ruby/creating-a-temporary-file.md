---
title:    "Ruby: Oppretting av en midlertidig fil"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

I Ruby-programmering, kan det være nyttig å ha midlertidige filer som kan brukes i korte øyeblikk. Dette kan være for å lagre midlertidige data eller for å utføre en bestemt oppgave uten å endre eksisterende filer. Å opprette midlertidige filer er en enkel og nyttig måte å organisere og strukturere koden din på.

## Hvordan

Det er to måter å opprette midlertidige filer i Ruby på. Den første er å bruke Ruby's standardbibliotek, "tempfile". Den andre er å bruke "File.open" og spesifisere "File::CREAT |  File::RDWR | File::EXCL" flagg for å sikre at filen er midlertidig og eksklusiv for din Ruby-prosess.

```Ruby
# Å opprette en midlertidig fil med tempfile biblioteket
require 'tempfile'
tempfile = Tempfile.new('midlertidig_fil')

# Å opprette en midlertidig fil med File.open
tempfile = File.open('temp.rb', 'w+')

# Å skrive til en midlertidig fil
tempfile.puts "Dette er en midlertidig fil"
tempfile.close

# Å lese fra en midlertidig fil
File.foreach("temp.rb") { |line| puts line }

```

Etter å ha utført koden over, vil du få følgende output:

```
Dette er en midlertidig fil
```

## Dypdykk

Når du oppretter en midlertidig fil i Ruby, vil filen bli slettet automatisk når den lukkes eller når programmet avsluttes. Dette er nyttig for å forhindre rot i filsystemet ditt med unødvendige filer. Du kan også spesifisere en mappe hvor midlertidige filer skal plasseres ved å bruke "tempfile.path" metoden.

Det er også viktig å merke seg at du må slette midlertidige filer manuelt hvis du skal bruke dem igjen. Dette kan gjøres ved å bruke "tempfile.unlink" metoden.

## Se også

- [Ruby's tempfile bibliotek](https://ruby-doc.org/stdlib/libdoc/tempfile/rdoc/Tempfile.html)
- [Ruby's File klassedokumentasjon](https://ruby-doc.org/core-2.7.1/File.html)
- [Offisiell Ruby-dokumentasjon](https://www.ruby-lang.org/no/documentation/)
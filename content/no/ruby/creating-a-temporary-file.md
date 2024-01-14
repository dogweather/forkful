---
title:    "Ruby: Lage en midlertidig fil"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Hvorfor

Å lage midlertidige filer kan være nødvendig når du jobber med programmering, spesielt når det kommer til å behandle store mengder data. Det kan også være nyttig når du ønsker å lagre midlertidig informasjon som ikke trenger å lagres permanent.

## Hvordan

For å opprette en midlertidig fil i Ruby, kan du bruke det innebygde biblioteket "tempfile". Her er et eksempel:

```Ruby
require 'tempfile'

# Opprett en midlertidig fil
tmp_file = Tempfile.new('my_temp_file')

# Skriv noen data til filen
tmp_file.puts "Dette er midlertidig informasjon"

# Lukk filen
tmp_file.close

# Les data fra filen
puts File.read(tmp_file.path)
```

Du kan også bruke en blokk for å sikre at filen lukkes automatisk:

```Ruby
require 'tempfile'

# Opprett en midlertidig fil
Tempfile.open('my_temp_file') do |tmp|
  # Skriv data til filen
  tmp.puts "Noe annet midlertidig informasjon"

  # Les data fra filen
  puts tmp.read

  # Filen lukkes automatisk når blokken er ferdig
end
```

## Dypdykk

Når du oppretter en midlertidig fil, vil den automatisk bli slettet når programmet ditt avsluttes. Filen vil også bli slettet om den ikke er lukket før programmet avsluttes.

Du kan også spesifisere hvor filen skal bli opprettet og hva slags filtype den skal være:

```Ruby
require 'tempfile'

# Opprett en midlertidig fil i en spesifisert mappe
tmp_file = Tempfile.new('my_temp_file', './path/to/folder/')

# Opprett en midlertidig fil med .txt filtype
tmp_file = Tempfile.new(['my_temp_file', '.txt'])

# Opprett en midlertidig fil i temp-mappen til systemet
tmp_file = Tempfile.new

# Skriv til og les fra filen som vanlig
```

Husk også å være oppmerksom på eventuelle sikkerhetsrisikoer når du jobber med midlertidige filer. Sørg for å slette filen når den ikke lenger er nødvendig for å unngå uautorisert tilgang til informasjonen.

## Se også

- [Ruby-dokumentasjon for Tempfile](https://ruby-doc.org/stdlib-2.6.3/libdoc/tempfile/rdoc/Tempfile.html)
- [Artikkel om å opprette midlertidige filer i Ruby](https://www.rubyguides.com/2016/09/ruby-tempfile/)
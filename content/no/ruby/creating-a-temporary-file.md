---
title:                "Ruby: Å lage en midlertidig fil"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Hvorfor

Å lage midlertidige filer er nyttig når du trenger å lagre data midlertidig, uten å lagre det permanent på din maskin. Dette kan være nyttig for oppgaver som krever midlertidig lagring av data, som for eksempel å lage backup filer eller lagre midlertidige nedlastinger.

# Hvordan

For å lage en midlertidig fil i Ruby, kan du bruke `Tempfile` biblioteket. Dette lar deg enkelt opprette en midlertidig fil og utføre handlinger på den.

```ruby
require 'tempfile'

# Opprett en ny midlertidig fil
temp_file = Tempfile.new('midlertidig_fil')

# Skriv til midlertidig fil
temp_file.write("Dette er en midlertidig fil!")

# Les innholdet fra midlertidig fil
temp_file.rewind
puts temp_file.read # Output: Dette er en midlertidig fil!

# Lukk midlertidig fil
temp_file.close

# Slett midlertidig fil
temp_file.unlink
```

I koden over bruker vi `Tempfile.new` for å opprette en midlertidig fil og gir den en navnestreng som parameter. Deretter skriver vi til filen ved hjelp av `write` metoden, og henter ut innholdet ved å bruke `read` metoden. Til slutt lukker vi filen og sletter den ved å bruke `unlink` metoden.

# Dypdykk

Når du oppretter en midlertidig fil ved hjelp av `Tempfile` biblioteket, blir filen lagret i et midlertidig område på din maskin. Dette området varierer avhengig av operativsystemet ditt, men filen vil automatisk bli slettet når programmet avsluttes. Du kan også velge å slette filen manuelt ved hjelp av `unlink` metoden.

En annen nyttig funksjon ved `Tempfile` biblioteket er at det automatisk oppretter og håndterer unike filnavn for deg. Dette kan være nyttig for å unngå navnekonflikter hvis du oppretter flere midlertidige filer på samme tid.

# Se også

- [Ruby dokumentasjon for Tempfile](https://ruby-doc.org/stdlib-2.7.2/libdoc/tempfile/rdoc/Tempfile.html)
- [Artikkel: Creating Temporary Files in Ruby](https://medium.com/better-programming/creating-temporary-files-in-ruby-339811f77562)
- [Artikkel: How to Use Temporary Files in Ruby](https://www.rubyguides.com/2018/09/temporary-files-in-ruby/)
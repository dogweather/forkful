---
title:                "Lage en midlertidig fil"
html_title:           "Ruby: Lage en midlertidig fil"
simple_title:         "Lage en midlertidig fil"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å opprette en midlertidig fil er en vanlig teknikk blant programmerere for å midlertidig lagre data mens programmet kjører. Dette kan være nyttig for å spare plass og forbedre programytelsen. 

## Hvordan:

For å opprette en midlertidig fil i Ruby, kan du bruke "Tempfile" biblioteket. Det krever bare en linje med kode og vil automatisk opprette en unik fil med et tilfeldig navn. Se eksempler nedenfor: 

```Ruby 
require 'tempfile' #Importer Tempfile biblioteket

#Opprette en midlertidig fil og skrive til det 
tf = Tempfile.new('mitt_tidligere_filnavn') 
tf.write("Dette er en midlertidig fil") 
puts tf.path # Skriver ut plassen til filen
tf.close # Lukker filen 
```

Eksempelet ovenfor vil opprette en midlertidig fil med navnet "mitt_tidligere_filnavn" og skrive teksten "Dette er en midlertidig fil" til den. Deretter vil den skrive ut plassen til filen og lukke den. 

Du kan også bruke "open" metoden fra Tempfile biblioteket for å åpne en midlertidig fil og skrive til den. Se eksempel nedenfor: 

```Ruby 
require 'tempfile' 

#Opprette en midlertidig fil og skrive til det 
Tempfile.open('mitt_tidligere_filnavn') do |tf| 
  tf.write("Dette er en midlertidig fil") 
  puts tf.path # Skriver ut plassen til filen 
end #Fil lukkes automatisk etter blokken er ferdig
```

Denne metoden gjør at filen automatisk lukkes når blokken er ferdig, så du trenger ikke å huske å lukke den manuelt. 

## Deep Dive:

Opprettelse av midlertidige filer har vært en vanlig praksis i programmering i mange år. Det er et nyttig verktøy for å håndtere midlertidige data og forbedre programmet ditt sin effektivitet. Alternativer til å opprette en midlertidig fil kan inkludere bruk av minnekort, database eller å lagre data i en variabel. 

Implementeringen av Tempfile biblioteket i Ruby bruker operativsystemets API for å generere et tilfeldig navn og opprette en midlertidig fil. Dette gjør det enkelt å bruke og gir pålitelig funksjonalitet. 

## Se også: 

For mer informasjon om bruk av Tempfile i Ruby, sjekk ut dokumentasjonen her: https://ruby-doc.org/stdlib-2.6.3/libdoc/tempfile/rdoc/Tempfile.html.
---
title:                "Ruby: Å lage en midlertidig fil"
simple_title:         "Å lage en midlertidig fil"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange ganger i programmering må vi opprette midlertidige filer for å lagre data midlertidig mens vi jobber med dem. Dette kan være for å behandle store mengder data eller for å dele informasjon mellom forskjellige deler av koden. Uansett årsak, er det viktig å vite hvordan man kan opprette midlertidige filer i Ruby for å holde koden ryddig og effektiv.

## Slik gjør du det

For å opprette en midlertidig fil i Ruby, bruker vi Tempfile-biblioteket. Først må vi importere Tempfile-biblioteket ved å skrive følgende kode øverst i koden vår:

```Ruby 
require 'tempfile' 
```

Deretter kan vi bruke `Tempfile.new`-metoden til å opprette en ny midlertidig fil. Denne metoden tar to argumenter, et navn og en sti. Hvis ingen argumenter gis, vil filen få et generisk navn og bli lagret i operativsystemets midlertidige mappe. Hvis vi for eksempel vil ha en midlertidig fil med navnet "data.txt" i hjemmemappen vår, kan vi skrive følgende:

```Ruby 
tempfile = Tempfile.new('data', 'home/brukernavn') 
```

Vi kan nå skrive til denne midlertidige filen ved å bruke `puts`-kommandoen og lukke filen ved å bruke `close`-metoden. Når programmet er ferdig, vil midlertidige filer automatisk bli slettet, så vi trenger ikke å bekymre oss for å rydde dem opp manuelt.

## Dykk dypere

Hvis vi vil ha mer kontroll over hvor midlertidige filer blir lagret, kan vi bruke `Tempfile.create`-metoden. Denne metoden lar oss spesifisere hvor filen skal lagres og gir oss en mer fleksibel måte å håndtere midlertidige filer på. Vi kan også bruke `Tempfile`-klassen til å opprette midlertidige mapper og slette dem etter behov.

## Se også

- [Ruby's Tempfile Library](https://ruby-doc.org/stdlib-2.6.4/libdoc/tempfile/rdoc/Tempfile.html)
- [Official Ruby Documentation on Tempfile](https://devdocs.io/ruby~2.5.0/tempfile)
- [How to Use Temporary Files in Ruby](https://til.hashrocket.com/posts/36a4942b57-how-to-use-temporary-files-in-ruby)
---
title:                "Ruby: Å lese en tekstfil."
simple_title:         "Å lese en tekstfil."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Hvorfor lese en tekstfil?

Tekstfiler er en essensiell del av mange programmeringsprosjekter. De inneholder ofte viktig informasjon som må leses og behandles av et program. Derfor er det viktig å forstå hvordan man kan lese en tekstfil i Ruby.

# Hvordan lese en tekstfil i Ruby

Å lese en tekstfil i Ruby er enkelt og kan gjøres ved hjelp av noen få linjer med kode. Først må du åpne tekstfilen ved å bruke File-klassen og passere inn filnavnet som et argument. Deretter kan du lese innholdet av filen ved å bruke metoden "read" på filobjektet.

```Ruby
file = File.open("tekstfil.txt", "r")
puts file.read
```

I dette eksempelet bruker vi "puts" metoden for å skrive ut innholdet av tekstfilen til konsollen. Du kan også bruke "gets" metoden for å lese innholdet linje for linje.

```Ruby
file = File.open("tekstfil.txt", "r")
file.each_line do |line|
  puts line
end
```

# Utforske en tekstfil

Når du har åpnet en tekstfil og lest innholdet, kan du utforske det videre ved hjelp av forskjellige Ruby-metoder. Du kan for eksempel bruke "split" metoden for å dele innholdet av filen ved hjelp av et bestemt tegn, og deretter utføre operasjoner på de ulike delene.

```Ruby
file = File.open("tekstfil.txt", "r")
content = file.read.split(",")
puts content[0] # Første del av tekstfilen
puts content[1] # Andre del av tekstfilen
```

En annen nyttig metode som kan brukes er "lines", som vil dele innholdet av filen ved hjelp av linjeskift og returnere en array av linjer. Dette kan være nyttig hvis du vil utføre spesifikke oppgaver på ulike deler av teksten.

```Ruby
file = File.open("tekstfil.txt", "r")
lines = file.read.lines
puts lines.length # Antall linjer i tekstfilen
```

# Se også

* [Ruby File-klassen dokumentasjon](https://ruby-doc.org/core-2.7.1/File.html)
* [Ruby String-klassen dokumentasjon](https://ruby-doc.org/core-2.7.1/String.html)
* [Video: How to Read a Text File in Ruby](https://www.youtube.com/watch?v=TKBPeIlkQWs)
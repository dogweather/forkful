---
title:                "Skriving til standardfeil"
html_title:           "Ruby: Skriving til standardfeil"
simple_title:         "Skriving til standardfeil"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

##Hvorfor

Mange ganger ønsker vi å vise meldinger eller feilmeldinger til brukeren mens programmet vårt kjører. Dette kan være nyttig for å forklare hva som skjer eller informere om eventuelle problemer. Å skrive til standard error gir en enkel og effektiv måte å gjøre akkurat det.

##Slik gjør du det

For å skrive til standard error, kan du bruke metoden `puts` og skrive ut meldingen din etterfulgt av `STDERR` som et argument. For eksempel:

```Ruby
puts "Dette er en melding til brukeren" + STDERR
```

Dette vil skrive ut meldingen til brukeren og vises som en feilmelding på skjermen. Du kan også bruke `warn` metoden, som gjør det samme som `puts` metoden, men bare for standard error. For eksempel:

```Ruby
warn "Dette er en feilmelding" # Vil skrive ut meldingen på standard error
```

Du kan også skrive ut variabelverdier i meldinger ved hjelp av string interpolasjon. For eksempel:

```Ruby
name = "John"
warn "Hei #{name}, velkommen til programmet vårt!"
```

Dette vil skrive ut "Hei John, velkommen til programmet vårt!" på standard error.

##Dykk dypere

Standard error er en spesiell filstrøm som brukes for å sende feilmeldinger til brukeren. Det er anbefalt å bruke denne strømmen for alle ikke-kritiske meldinger og feilmeldinger, mens standard out bør brukes for den vanlige utdataen til programmet.

En annen grunn til å bruke standard error er at det kan kobles til et loggebibliotek for å lagre alle meldinger og feilmeldinger samlet på ett sted. Dette kan være nyttig for feilsøking av problemer i et større prosjekt.

##Se også

- [Ruby dokumentasjon for StandardError](https://ruby-doc.org/core-2.7.0/StandardError.html)
- [Enkel guide til Ruby exceptions](https://www.rubyguides.com/2019/03/ruby-exceptions/)
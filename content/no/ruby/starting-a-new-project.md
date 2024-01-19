---
title:                "Starter et nytt prosjekt"
html_title:           "Arduino: Starter et nytt prosjekt"
simple_title:         "Starter et nytt prosjekt"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å starte et nytt prosjekt handler om å initieres arbeid med en ny idé eller å løse et nytt problem. Programmerere gjør dette for å bygge nye applikasjoner, teste teorier eller lære nye ferdigheter. 

## Hvordan:

Her starter vi et nytt Ruby-prosjekt:

```Ruby
# Installer Ruby hvis du ikke allerede har det
$ sudo apt-get install ruby-full

# Lag en ny katalog for prosjektet ditt
$ mkdir mitt_nye_prosjekt

# Gå inn i prosjektkatalogen
$ cd mitt_nye_prosjekt

# Lag en ny Ruby-fil
$ touch hoved.rb

# Åpne filen i tekstredigeringsprogrammet
$ nano hoved.rb
```

Skriv inn følgende i `hoved.rb`:

```Ruby
puts "Hei, verden!"
```

For å kjøre programmet:

```Ruby
$ ruby hoved.rb
```

Resultatet skal være:

`Hei, verden!`

## Dyp Dykk

### Historie

Ruby har vært i omløp siden 1995, med sikte på å gi programmereren en effektiv og underholdende opplevelse. Dens syntax ble påvirket av Perl, Smalltalk, Eiffel, Ada og Lisp.

### Alternativer

Det er mange andre programmeringsspråk du kan bruke til å starte et nytt prosjekt. Noen eksempler er Python, Java, JavaScript, C#, PHP, Swift eller Go. Valget avhenger av kravene til prosjektet, dine personlige preferanser og arbeidsplassen sin standard.

### Implementeringsdetaljer

Ruby er et tolket, høy-nivå, generelt bruk programmeringsspråk. Det betyr at Ruby-koden din kjøres linje for linje, så du kan enkelt teste og justere koden din.

## Se Også

1. [Offisiell Ruby-Dokumentasjon](https://www.ruby-lang.org/en/documentation/)
2. [Yehuda Katz's Guide to Building Gems](https://yehudakatz.com/2010/12/16/clarifying-the-roles-of-the-gemspec-and-gemfile/)
3. [Praktisk Ruby for Systemansvarlige](https://www.apress.com/gp/book/9781484220633)

Husk, øvelse gjør mester. Lykke til med Ruby-prosjektet ditt!
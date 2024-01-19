---
title:                "Å starte et nytt prosjekt"
html_title:           "C: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "Bash"
category:             "Bash"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å starte et nytt prosjekt er å lage et unikt arbeidsområde hvor programmerarbeid kan utføres - tenk på det som en ny skisseblokk. Programmerere gjør dette for å organisere og isolere ulike prosjekter for effektivitets skyld.

## Hvordan man:

Å starte et nytt Bash-prosjekt er enkelt. Du kan opprette en ny mappe og deretter en bash-scriptfil med `.sh`-utvidelsen:

```Bash
mkdir MyProject
cd MyProject
touch script.sh
```

Nå kan du begynne å skrive ditt script! Si hei til verden med:

```Bash
echo "#!/bin/sh" > script.sh
echo "echo Hei Verden!" >> script.sh
```

Kjør scriptet med:

```Bash
chmod +x script.sh
./script.sh
```

Utskriften vil være:

```Bash
Hei Verden!
```

## Dypdykk

Tidlig i Bash sin historie var prosjektseparasjon ikke så formell. Tidligere versjoner manglet funksjonaliteter for effektiv prosjektseparasjon. I dag, med moderne versjoner kan vi lage egne miljøer for hvert prosjekt.

Når det gjelder alternativer, er det mange andre shell scripting-språk som også støtter prosjektorganisering, inkludert Zsh, Fish, og flere andre.

Når det kommer til implementeringsdetaljer, så inkluderer et typisk Bash-prosjekt flere filer, inkludert koden, en README-fil for dokumentering, og noen ganger en installasjons- eller konfigureringsfil.

## Se også

Hvis du ønsker å fordype deg mer kan du sjekke ut disse ressursene:

- [The Bash Guide](http://guide.bash.academy/): En omfattende guide for å lære Bash fra grunnen av. 
- [Bash scripting cheatsheet](https://devhints.io/bash): Oppslagstavle for vanlige bash-kommandoer og konsepter.
- [Awesome Bash](https://awesome.re/t/bash): En kurert liste over Bash-relaterte ressurser.
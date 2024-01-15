---
title:                "Å lese en tekstfil"
html_title:           "Bash: Å lese en tekstfil"
simple_title:         "Å lese en tekstfil"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har brukt terminalen på datamaskinen din, har du kanskje lagt merke til at du kan gjøre mange ting ved hjelp av Bash-kommandoer. En vanlig oppgave i Bash er å lese en tekstfil. I denne artikkelen vil vi se på hvorfor du kanskje ønsker å lese en tekstfil, og hvordan du kan gjøre det.

## Hvordan gjøre det

Det er flere måter å lese en tekstfil på i Bash, avhengig av hva du vil gjøre med innholdet i filen. La oss ta en titt på noen eksempler ved hjelp av kodeblokker:

```
# Les hele filen
cat fil.txt

# Les de første 10 linjene i filen
head -n 10 fil.txt

# Les de siste 10 linjene i filen
tail -n 10 fil.txt

# Les en bestemt linje i filen, for eksempel linje 15
sed -n '15p' fil.txt

# Søk etter et bestemt mønster i filen og les linjene som matcher
grep "søkeord" fil.txt
```

Disse er bare noen få eksempler på kommandoer du kan bruke for å lese en tekstfil i Bash. Du kan også kombinere kommandoer for å få mertraktive resultater, for eksempel å søke etter et bestemt mønster og deretter bare lese linjene som inneholder dette mønsteret. Det finnes mange flere muligheter avhengig av hva du prøver å oppnå.

## Dypdykk

Hvis du er interessert i å lære mer om hvordan Bash leser tekstfiler, er det viktig å forstå at Bash betrakter alle filer som en tekstfil. Det betyr at kommandoene vi brukte i forrige seksjon også kan brukes på andre typer filer, som for eksempel en CSV-fil. I tillegg til standardkommandoene finnes det også spesialkommandoer for å lese og behandle tekstfiler. Noen eksempler på disse er `awk`, `cut`, `sort` og `tr`.

Det kan også være lurt å ha en forståelse av hvordan Bash håndterer linjeskift (engelsk: line breaks) i tekstfiler. I Bash, og de fleste andre programmeringsspråk, betraktes linjeskift som en spesiell karakter som angir slutten på en linje i en tekstfil. Dette er viktig å være klar over når du leser og behandler tekstfiler.

## Se også

- [Bash Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash Cheatsheet](https://devhints.io/bash)
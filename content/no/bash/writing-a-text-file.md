---
title:                "Å skrive en tekstfil"
html_title:           "Bash: Å skrive en tekstfil"
simple_title:         "Å skrive en tekstfil"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive en tekstfil er en grunnleggende ferdighet som er nyttig for alle som ønsker å lære å programmere. Det lar deg lage og organisere informasjon på en enkel og strukturert måte.

## Hvordan du gjør det

Det første du må gjøre er å åpne terminalen din og starte Bash-skallet ved å skrive "bash" og trykke på enter. Deretter kan du følge disse enkle trinnene for å lage en tekstfil:

1. Naviger til mappen der du ønsker å lagre tekstfilen ved å bruke kommandoen "cd".

```Bash
cd Documents/
```

2. Skriv kommandoen "touch" etterfulgt av navnet du vil gi til tekstfilen din.

```Bash
touch min-fil.txt
```

3. Åpne tekstfilen ved å bruke kommandoen "nano" etterfulgt av filnavnet.

```Bash
nano min-fil.txt
```

4. Skriv inn teksten du vil ha i filen og lagre ved å trykke på "Ctrl + X", skriv "Y" for å bekrefte endringer, og trykk på enter for å lagre filen.

5. For å se innholdet i tekstfilen, bruk kommandoen "cat".

```Bash
cat min-fil.txt
```

Her er et eksempel på hva du kan få som output:

```Bash
Dette er en tekstfil.
Jeg har lagt til litt tekst.
``` 

## Dykke dypere

Nå som du har lært det grunnleggende om å lage en tekstfil, kan du også utforske forskjellige kommandoer for å manipulere tekst, som "grep" og "awk". Du kan også bruke tekstfiler til å lagre variabler og utføre beregninger i Bash-skallet.

## Se også

- [https://www.gnu.org/software/bash/](https://www.gnu.org/software/bash/)
- [https://www.howtogeek.com/435903/learn-the-basics-of-bash-scripting-and-how-to-build-a-shell-script/](https://www.howtogeek.com/435903/learn-the-basics-of-bash-scripting-and-how-to-build-a-shell-script/)
- [https://www.linuxjournal.com/content/bash-tips-using-bash-awk-and-grep-efficient-text-file-processing](https://www.linuxjournal.com/content/bash-tips-using-bash-awk-and-grep-efficient-text-file-processing)
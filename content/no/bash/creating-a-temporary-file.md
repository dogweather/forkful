---
title:                "Bash: Oppretting av midlertidig fil"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lage midlertidige filer i Bash-programmering kan være nyttig for å lagre data eller informasjon som kun trengs midlertidig i en kode. Dette kan være en nyttig teknikk for å holde koden din ren og organisert.

## Hvordan lage en midlertidig fil

For å opprette en midlertidig fil i Bash, kan du bruke `mktemp` kommandoen. Denne kommandoen oppretter automatisk en unik midlertidig fil og returnerer stien til filen slik at du kan bruke den videre i koden din. Her er et eksempel på å lage en midlertidig fil og skrive noe innhold i den:

```Bash
temp_file=$(mktemp)
echo "Dette er en midlertidig fil" > $temp_file

echo $temp_file # Skriver ut stien til den midlertidige filen
cat $temp_file # Skriver ut innholdet i den midlertidige filen
```

Dette vil produsere følgende output:

```Bash
/tmp/tmp.qrK2TS32
Dette er en midlertidig fil
```

`mktemp` lar deg også spesifisere et prefiks for filnavnet, slik at du kan gi et mer meningsfylt navn til den midlertidige filen. For eksempel:

```Bash
prefiks="temp"
temp_file=$(mktemp -t $prefix)
```

Den midlertidige filen vil da følge navnekonvensjonen `temp.XXXXXX`.

## Deep Dive

Når en midlertidig fil er opprettet, vil den automatisk bli slettet når programmet er ferdigkjørt. Dette er nyttig for å unngå rot og forebygge personvernproblemer. Hvis du imidlertid ønsker å beholde den midlertidige filen for å undersøke dataene senere, kan du bruke `trap` kommandoen for å fange opp signaler som utløses når programmet avsluttes.

For eksempel kan du bruke følgende kode for å fjerne den midlertidige filen når programmet avsluttes, men beholde den hvis programmet avsluttes med `CTRL+C`:

```Bash
cleanup() {
  rm $temp_file
}

trap cleanup EXIT # Kjør cleanup-funksjonen når programmet avsluttes

# Resten av koden din her
```

Du kan også bruke `mktemp` kommandoen til å opprette midlertidige mapper ved å bruke flagget `-d`.

## Se også

- [Official Bash documentation on mktemp](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-mktemp-378) (Offisiell Bash-dokumentasjon om mktemp)
- [An introduction to Bash scripting](https://opensource.com/resources/bash) (En introduksjon til Bash-skripting)
---
title:                "Lage en midlertidig fil"
html_title:           "Bash: Lage en midlertidig fil"
simple_title:         "Lage en midlertidig fil"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor
 Å lage midlertidige filer kan være nyttig når du trenger å lagre midlertidige data eller utføre en kompleks handling som krever flere trinn.

## Hvordan
Å lage en midlertidig fil i Bash er enkelt. Du trenger bare å bruke kommandoen `mktemp` etterfulgt av en filnavn-mal, for eksempel `tmp.XXXXXX`. Dette vil opprette en midlertidig fil med et unikt navn. Se eksemplet nedenfor:

```Bash
tmpfile=$(mktemp tmp.XXXXXX)
echo "Dette er en midlertidig fil" > $tmpfile
cat $tmpfile
```
**Output:**
```
Dette er en midlertidig fil
```

For å sikre at den midlertidige filen blir slettet, selv om skriptet ditt feiler, bør du bruke `trap` kommandoen til å fange opp feil og slette filen. Se eksemplet nedenfor:

```Bash
tmpfile=$(mktemp tmp.XXXXXX)

trap "rm -f $tmpfile" EXIT # sletter filen når skriptet avsluttes

# kode for å behandle den midlertidige filen

# hvis en feil oppstår, vil filen bli slettet av trap-kommandoen
```

## Dykk dypere
Det finnes forskjellige alternativer og måter å lage en midlertidig fil på i Bash. Du kan for eksempel bruke `mktemp -p directory` for å angi en bestemt katalog for å opprette filen i. `mktemp` kommandoen har også en rekke andre nyttige alternativer som kan være verdt å utforske.

## Se også
- [Bash Manual: mktemp](https://www.gnu.org/software/bash/manual/html_node/The-Restrict-Alternate-Console.html)
- [Linux Journal: Temporary Files in Shell Scripts](https://www.linuxjournal.com/content/using-temporary-files-shell-scripts)
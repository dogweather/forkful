---
title:    "Fish Shell: Lage en midlertidig fil"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor?

Når du skriver programmer, er det ofte behov for å opprette midlertidige filer for å lagre midlertidig informasjon som ikke er nødvendig å lagre permanent på datamaskinen din. Dette kan være nyttig for å behandle store datasett eller for å utføre komplekse oppgaver uten å overskride grenser for tilgjengelig minne. Å opprette midlertidige filer kan også være nyttig når du ønsker å opprettholde sikkerheten og rensligheten til filene dine ved bare å beholde de filene som er nødvendige for det endelige programmet.

## Hvordan

Fish Shell har en innebygd funksjon for å opprette midlertidige filer. Denne funksjonen er veldig enkel å bruke og krever minimal mengde kode.

```Fish Shell
set filename (mktemp)  # Opprett en midlertidig fil og lagre den under variabelen "filename"
echo "Dette er en midlertidig fil" > $filename  # Skriv tekst til den midlertidige filen
cat $filename  # Vis innholdet i den midlertidige filen
rm $filename  # Slett den midlertidige filen
```

Det første trinnet er å bruke kommandoen "mktemp" for å opprette en unik og midlertidig fil. Denne filen vil være tom når den først blir opprettet. Deretter kan du bruke variabelen til å utføre ulike operasjoner på den midlertidige filen. I dette eksempelet vises hvordan du skriver tekst til filen, viser innholdet og til slutt sletter filen ved å bruke kommandoen "rm".

## Dypdykk

Når du oppretter en midlertidig fil, vil den bli lagret i en midlertidig katalog som er spesifisert i miljøvariabelen "TMPDIR". Du kan sjekke denne variabelen ved å kjøre kommandoen "echo $TMPDIR" i terminalen. Hvis du ikke allerede har en miljøvariabel satt for "TMPDIR", vil den bli lagret i /tmp-katalogen som standard.

En annen god praksis ved å opprette midlertidige filer er å legge til et kort identifikasjonsnummer i filnavnet. Dette sikrer at filen er enestående og unngår eventuelle konflikter hvis du oppretter flere midlertidige filer i samme program.

## Se også

- [The mktemp command in Fish Shell](https://fishshell.com/docs/current/cmds/mktemp.html)
- [How to create a temporary file in Bash](https://linuxhint.com/create_temporary_file_bash/)
- [Creating and removing temporary files](https://askubuntu.com/questions/634796/creating-and-removing-temporary-files)
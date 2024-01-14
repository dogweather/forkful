---
title:    "Bash: Sletting av tegn som matcher et mønster"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Hvorfor

Å slette tegn som matcher et mønster er en vanlig oppgave i Bash-programmering, spesielt når du jobber med tekstbehandling. Dette kan være nyttig for å fjerne uønskede tegn eller formatering fra en tekstfil eller for å filtrere ut spesifikke data.

## Hvordan gjøre det

Det finnes forskjellige måter å slette tegn som matcher et mønster på i Bash, men den vanligste metoden er å bruke verktøyet "sed" (stream editor). Her er et eksempel på hvordan dette kan gjøres:

```Bash
sed 's/mønster//g' filnavn.txt
```

I dette eksempelet vil alle forekomster av "mønster" i filen "filnavn.txt" bli slettet. Hvis du ønsker å slette et bestemt sett med tegn i stedet for å erstatte dem med intet, kan du bruke følgende kommando:

```Bash
sed 's/[tegnsett]//g' filnavn.txt
```

Her vil "[tegnsett]" bli erstattet med det spesifikke tegnsettet du ønsker å slette. For eksempel, hvis du ønsker å slette alle tall i en tekstfil, kan du bruke "[0-9]" som tegnsett.

## Dypdykk

Når du bruker "sed" til å slette tegn som matcher et mønster, er det viktig å være klar over at dette vil påvirke alle forekomster av mønsteret i filen. Hvis du bare ønsker å slette første forekomst av et mønster, kan du bruke flagget "-i" for å begrense endringen til den første forekomsten:

```Bash
sed -i 's/mønster//1' filnavn.txt
```

Dette vil slette første forekomst av "mønster" i filen.

Du kan også bruke dette til å begrense endringene til et bestemt område i filen ved å bruke et linjenummer:

```Bash
sed -i 's/mønster//nummer' filnavn.txt
```

Dette vil bare slette første forekomst av "mønster" i linje "nummer" i filen.

## Se også

- [Sed Command in Linux/Unix with examples](https://www.geeksforgeeks.org/sed-command-in-linux-unix-with-examples/)
- [Delete Characters with sed](https://www.baeldung.com/linux/delete-characters-sed)
- [Bash sed - How to Delete Characters from a String](https://linuxhint.com/bash_sed_delete_characters_string/)
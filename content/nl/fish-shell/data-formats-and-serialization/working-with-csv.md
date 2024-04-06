---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:08.261235-07:00
description: 'Hoe te: 1. Een CSV-bestand regel voor regel lezen.'
lastmod: '2024-04-05T21:53:51.276820-06:00'
model: gpt-4-0125-preview
summary: ''
title: Werken met CSV
weight: 37
---

## Hoe te:
1. Een CSV-bestand regel voor regel lezen:
```Fish Shell
for line in (cat file.csv)
    echo $line
end
```

2. Velden splitsen en een specifieke kolom afdrukken (bijv. de tweede kolom):
```Fish Shell
cat file.csv | while read -l line
    set -l fields (string split "," $line)
    echo $fields[2]
end
```

3. Naar een CSV-bestand schrijven:
```Fish Shell
echo "naam,leeftijd,stad" > gebruikers.csv
echo "Alice,30,New York" >> gebruikers.csv
echo "Bob,25,Los Angeles" >> gebruikers.csv
```

Voorbeeldoutput (inhoud van `gebruikers.csv`):
```
naam,leeftijd,stad
Alice,30,New York
Bob,25,Los Angeles
```

## Diepgaande Verkenning
De omgang met CSV bestaat al sinds de vroege dagen van persoonlijke computers en is geëvolueerd als een eenvoudig formaat voor gegevensuitwisseling. Hoewel basis, kan het gebrek aan een standaard bij CSV leiden tot parseerissues, zoals verschillende scheidingsmethoden en tekstcoderingen. Terwijl Fish Shell geen ingebouwde CSV parseertools heeft, worden `awk`, `sed`, en `cut` vaak naast het gebruik ervan ingezet voor meer complexe taken.

De benadering van Fish tot CSV is meer handmatig en script-gebaseerd, gebruikmakend van zijn mogelijkheden voor stringmanipulatie om CSV-velden te beheren. Voor zware gegevensverwerking, overweeg alternatieven zoals Python's `pandas` bibliotheek, of command-line hulpprogramma's zoals `csvkit`.

## Zie Ook
- Aan de slag met `awk`: [AWK - Een tutorial en introductie](https://www.grymoire.com/Unix/Awk.html)
- Introductie tot `sed`: [Sed - Een introductie en tutorial](https://www.grymoire.com/Unix/Sed.html)
- Officiële Fish Shell Documentatie: [Fish Shell Documentatie](https://fishshell.com/docs/current/index.html)
- `csvkit` Documentatie: [csvkit - Een reeks hulpprogramma's voor het converteren naar en werken met CSV](https://csvkit.readthedocs.io/en/latest/)

---
title:                "Å jobbe med csv"
html_title:           "Fish Shell: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
CSV er et universelt format for å lagre og vise data, spesielt i tabeller med rader og kolonner. Det er mye brukt av programmerere for å behandle store mengder data.

# Hvordan:
```fish shell
# Lesing av CSV-fil
set rows (csv_reader rows.csv)

# Skrive ut kolonner av data fra CSV-fil
echo $rows[column1]
```

# Dykk ned:
CSV har vært i bruk siden 1970-tallet og er utviklet som en enkel og effektiv måte å lagre data på. Det finnes også andre formater som JSON og XML, men CSV er fortsatt populært på grunn av sin enkelhet og lesbarhet. Fish Shell tilbyr et innebygd kommandolinjeverktøy for å håndtere CSV-data, noe som gjør prosessen enklere for programmerere.

# Se også:
- For mer informasjon om å arbeide med CSV i Fish Shell, sjekk ut dokumentasjonen her: https://fishshell.com/docs/current/cmds/parse.html#csv
- Hvis du ønsker å utforske alternative måter å arbeide med CSV-filer på, kan du sjekke ut programmeringsspråk som Python og R som tilbyr spesifikke biblioteker for å håndtere CSV-data.
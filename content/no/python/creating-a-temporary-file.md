---
title:                "Python: Oppretting av midlertidig fil"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å lage midlertidige filer er en viktig del av programmering. Disse filene kan være nyttige for å lagre midlertidige data eller opprette midlertidige filnavn for senere bruk.

## Slik gjør du det

For å lage en midlertidig fil i Python, kan du bruke "tempfile" modulen. Her er et eksempel på hvordan du kan opprette og skrive til en midlertidig fil:

```Python
import tempfile

# Opprett en midlertidig fil i systemets midlertidige mappe
temp_file = tempfile.NamedTemporaryFile(delete=False)

# Skriv noen data til filen
temp_file.write(b'Dette er noen midlertidige data')

# Lukk filen
temp_file.close()

# Les dataene fra filen
with open(temp_file.name, 'r') as f:
    print(f.read())

# Output: Dette er noen midlertidige data
```

Her bruker vi "NamedTemporaryFile()" -funksjonen for å opprette en midlertidig fil og lagre filens objekt i en variabel. Vi bruker også "delete=False" -parameteren for å sikre at filen ikke blir slettet når den lukkes. Deretter skriver vi noen data til filen og lukker den. Til slutt kan vi åpne og lese filen ved hjelp av "open()" -funksjonen.

Det er også mulig å spesifisere et ønsket filnavn og mappe for den midlertidige filen ved hjelp av "NamedTemporaryFile()" -funksjonen. For mer informasjon om modulen "tempfile", kan du se dokumentasjonen [her](https://docs.python.org/3/library/tempfile.html).

## Deep Dive

Den midlertidige filen som blir opprettet i eksemplet ovenfor, vil bli automatisk slettet når filobjektet blir lukket. Dette skyldes at den midlertidige filen blir opprettet med en slettetagsparameter som standard.

Du kan også bruke "TemporaryFile()" -funksjonen i stedet for "NamedTemporaryFile()" hvis du ikke trenger å være i stand til å åpne filen senere. Dette vil opprette en midlertidig fil som blir slettet automatisk når den lukkes.

Det er også mulig å slette den midlertidige filen manuelt ved å bruke "unlink()" -funksjonen på filobjektet, eller ved å spesifisere "delete=True" -parameteren når du oppretter filen.

## Se også

- [Dokumentasjon for "tempfile" modulen på Python.org](https://docs.python.org/3/library/tempfile.html)
- [Tutorial: "Creating temporary files in Python" (Engelsk)](https://www.tutorialspoint.com/creating-temporary-files-in-python)
---
title:                "Opprette en midlertidig fil"
html_title:           "Python: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

### Hvorfor

Å skape midlertidige filer er en viktig del av programmering da det tillater oss å lagre data eller informasjon midlertidig under kjøring av et program. Dette kan være nyttig for å unngå å overskrive eller miste viktig data, og for å sikre at programmene våre kjører effektivt.

### Hvordan

Vi kan enkelt opprette en midlertidig fil i Python ved hjelp av det innebygde "tempfile" biblioteket. Følgende kode viser hvordan du kan opprette en temporær fil og skrive til den:

```Python
import tempfile

# Opprett en midlertidig fil
temp_file = tempfile.NamedTemporaryFile()

# Skriv til filen
temp_file.write(b"Dette er en midlertidig fil.")

# Lukk filen
temp_file.close()

# Les filens innhold
read_file = open(temp_file.name, "r")
print(read_file.read())

# Output: Dette er en midlertidig fil.
```

Vi kan også spesifisere navnet og plasseringen av den midlertidige filen ved hjelp av "NamedTemporaryFile()" funksjonen. Dette kan være nyttig når vi ønsker å lagre midlertidige filer på bestemte steder.

### Dypdykk

Når vi oppretter en midlertidig fil, blir den automatisk slettet når programmet vårt er ferdig med å kjøre. Dette eliminerer behovet for å manuelt slette filen og frigjør plass på datamaskinen vår.

Vi kan også spesifisere en "delete=False" parameter når vi oppretter den midlertidige filen for å unngå automatisk sletting. Dette kan være nyttig når vi ønsker å lagre data i den midlertidige filen for senere bruk.

### Se også

- Dokumentasjon for "tempfile" biblioteket i Python: https://docs.python.org/3/library/tempfile.html
- En praktisk guide for å lage midlertidige filer i Python: https://realpython.com/python-tempfile/
---
title:                "Python: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skape midlertidige filer er en essensiell del av programmering. Dette kan være nyttig når du midlertidig trenger å lagre data eller når du trenger et sted å mellomlagre informasjon mens programmet ditt kjører.

## Hvordan

For å opprette en midlertidig fil i Python, kan du bruke modulen "tempfile". Først importerer du modulen ved å skrive følgende linje i din Python-fil:

```Python
import tempfile
```

Deretter kan du bruke funksjonen "NamedTemporaryFile()" for å opprette en midlertidig fil og få en hånd på den:

```Python
temp_file = tempfile.NamedTemporaryFile()
```

Du kan også angi et navn på den midlertidige filen dersom du ønsker det:

```Python
temp_file = tempfile.NamedTemporaryFile(prefix="mitttempnavn_")
```

Når du er ferdig med å bruke den midlertidige filen, husk å slette den ved å bruke "close()" -funksjonen:

```Python
temp_file.close()
```

Du kan også skrive og lese fra den midlertidige filen akkurat som du ville gjort med en vanlig fil. For eksempel kan du bruke følgende kode for å skrive til filen:

```Python
temp_file.write("Dette er midlertidige filer i Python!")
```

For å lese fra filen, kan du bruke "read()" -funksjonen:

```Python
print(temp_file.read())
```

Når programmet ditt er ferdig med å kjøre, vil den midlertidige filen automatisk bli slettet.

## Dypdykk

Standard plasseringen for midlertidige filer er i "/tmp" -mappen på Unix og Linux-systemer, mens på Windows vil den bli opprettet i mappen specificert av den midlertidige miljøvariabelen.

Du kan også endre plasseringen og andre egenskaper for den midlertidige filen ved å spesifisere verdier for ulike parametere når du oppretter filen. Du kan for eksempel angi plasseringen ved å bruke "dir" -parameteren:

```Python
temp_file = tempfile.NamedTemporaryFile(dir="/min/midlertidig/mappe")
```

For å få en fullstendig liste over mulige parametere og deres betydning, kan du se dokumentasjonen for "tempfile" -modulen.

## Se også

- [Dokumentasjon for tempfile-modulen](https://docs.python.org/3/library/tempfile.html)
- [Guide for å håndtere midlertidige filer i Python](https://stackabuse.com/handling-temporary-files-in-python/)
- [Eksempelkode for å lage midlertidige filer i Python](https://www.geeksforgeeks.org/temporary-files-python/)
---
title:                "Skriving til standardfeil"
html_title:           "Python: Skriving til standardfeil"
simple_title:         "Skriving til standardfeil"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen ønske å skrive til standard error i Python? Vel, det er en veldig nyttig måte å få informasjon og feilmeldinger på mens du kjører programmet ditt. Det kan hjelpe deg med å identifisere og løse problemer som kan oppstå underveis.

## Hvordan gjøre det

For å skrive til standard error i Python, bruker du `sys.stderr.write()` funksjonen. Her er et eksempel:

```Python
import sys
sys.stderr.write("Dette er en feilmelding")
```

Dette vil skrive ut "Dette er en feilmelding" til standard error.

Du kan også bruke denne funksjonen til å vise andre typer informasjon, for eksempel:

```Python
import sys
sys.stderr.write("Denne informasjonen blir skrevet til standard error")
```

Dette vil skrive ut "Denne informasjonen blir skrevet til standard error" til standard error.

## Dypdykk

Nå som du vet hvordan du kan skrive til standard error, la oss ta en dypere titt på hva det egentlig betyr. Standard error (eller STDERR) er en strøm som brukes til å vise feilmeldinger og annen informasjon som kan være viktig for deg mens du kjører programmet ditt. Det er en del av det som kalles standard I/O (input/output) i Python. Standard I/O er en måte for datamaskiner å kommunisere med brukere, enheter og andre programmer.

## Se også

- [Python Dokumentasjon: sys.stderr.write()](https://docs.python.org/3/library/sys.html#sys.stderr)
- [W3schools: Standard I/O](https://www.w3schools.com/python/python_file_io.asp)
- [RealPython: Writing to Standard Error](https://realpython.com/python-logging/#writing-to-standard-error)
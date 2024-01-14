---
title:    "Python: Oppretting av en midlertidig fil"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Hvorfor
Temporære filer er nyttige når du trenger midlertidig lagring av data i et Python-program. Dette kan være for å lagre informasjon som blir generert under kjøring eller for å passere data til ulike deler av koden din.

# Hvordan
Det er enkelt å opprette en temporær fil i Python ved å bruke modulen "tempfile". Først må vi importere denne modulen inn i koden vår:

```Python
import tempfile
```

Deretter kan vi opprette en midlertidig fil ved å bruke "NamedTemporaryFile" -funksjonen:

```Python
temp_file = tempfile.NamedTemporaryFile()
```

Vi kan også spesifisere prefix og suffix for filnavnet:

```Python
temp_file = tempfile.NamedTemporaryFile(prefix="temp_", suffix=".txt")
```

Når vi er ferdig med å bruke filen, må vi huske å slette den ved å bruke "close" -funksjonen:

```Python
temp_file.close()
```

Hvis vi vil skrive data til den midlertidige filen, kan vi bruke "write" -funksjonen:

```Python
temp_file.write("Dette er en midlertidig fil.")
```

Når vi senere åpner filen, kan vi se at teksten har blitt skrevet til filen:

```Python
with open(temp_file.name) as file:
   print(file.read())
```

Output: 

```
Dette er en midlertidig fil.
```

# Dypdykk
Når vi oppretter en temporær fil, blir den automatisk slettet når programmet vårt er ferdig med å kjøre. Dette sikrer at vi ikke ender opp med en masse midlertidige filer som tar opp plass på datamaskinen vår.

Vi kan også bruke modulen "tempfile" til å opprette midlertidige mapper ved å bruke "TemporaryDirectory" -funksjonen. Denne mappen vil også bli slettet automatisk når programmet er ferdig.

En annen nyttig funksjon med modulen "tempfile" er "mkstemp", som lar oss opprette både et filnavn og en filobjekt samtidig.

# Se også
- [Python dokumentasjon for modulen "tempfile"](https://docs.python.org/3/library/tempfile.html)
- [Artikkel på Real Python om bruk av temporære filer i Python](https://realpython.com/python-tempfile/)
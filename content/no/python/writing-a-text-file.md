---
title:                "Python: Skrive en tekstfil"
simple_title:         "Skrive en tekstfil"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

#Hvorfor

Å skrive en tekstfil kan være en nyttig ferdighet å lære for de som ønsker å programmere. Det lar deg lagre data permanent og kan brukes til å opprette konfigurasjonsfiler, loggfiler og mye mer.

#Hvordan

```python
# Åpne en tekstfil for skriving
fil = open("minfil.txt", "w")

# Skriv data til filen
fil.write("Dette er en tekst som skal skrives til filen.")

# Lukk filen
fil.close()
```

```python
# Åpne en tekstfil for lesing
fil = open("minfil.txt", "r")

# Les hele filen
data = fil.read()

# Skriv ut data
print(data)

# Lukk filen
fil.close()
```

**Output:**
Dette er en tekst som skal skrives til filen.

#Dypdykk

Når du skriver til en tekstfil, må du huske å lukke filen for å sikre at dataene dine blir lagret. Du kan også bruke "with" uttrykket for å sørge for automatisk lukking av filen når du er ferdig med å jobbe med den.

```python
# Åpne en tekstfil for skriving
with open("minfil.txt", "w") as fil:

# Skriv data til filen
fil.write("Dette er en tekst som skal skrives til filen.")
```

Du kan også skrive til en tekstfil ved hjelp av "print" funksjonen i Python. Dette er nyttig når du vil formatere utdataene dine eller legge til nye linjer.

```python
# Skriv til filen med print
print("Dette er en annen linje som blir skrevet til filen.", file=fil)
```

Du kan også spesifisere et annet tegnsett når du åpner filen for å sikre at spesielle tegn og bokstaver blir skrevet riktig.

```python
# Åpne filen for skriving med UTF-8 tegnsett
with open("minfil.txt", "w", encoding="utf-8") as fil:
```

#Se også

- [Python dokumentasjon om åpning og lukking av filer](https://docs.python.org/no/3/library/functions.html#open)
- [Tutorial om å skrive til tekstfiler i Python](https://realpython.com/read-write-files-python/)
- [Enkel guide om å håndtere filer i Python](https://www.programiz.com/python-programming/file-operation)
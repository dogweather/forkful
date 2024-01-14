---
title:                "Python: Utskrift av feilsøkingsutdata"
programming_language: "Python"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive et Python-program kan noen ganger føles som å navigere i en mørk labyrint. Du prøver å finne ut hvorfor koden ikke fungerer som den skal, og hvor nøyaktig feilen skjer. En måte å lettere guide seg gjennom labyrinten er ved å bruke print-setninger for å få output av verdier og sjekke om de er som forventet. Dette kan spare deg for mye tid og frustrasjon når du feilsøker koden din.

## Hvordan

For å printe ut debug output i Python, bruker du funksjonen `print()`. Du kan skrive ut verdier eller variabler ved å plassere dem inne i parentesene til `print()`.

```
# Eksempel på bruk av print() for å skrive ut en streng:
print("Hei, verden!")
# Output: Hei, verden!

# Eksempel på bruk av print() for å skrive ut en variabel:
navn = "Marie"
print(f"Hei, {navn}!")
# Output: Hei, Marie!
```

Hvis du vil printe ut flere verdier eller variabler på en linje, kan du skille dem med komma.

```
# Eksempel på bruk av print() for å printe ut flere verdier på en linje:
print("Alder:", 25, "år")
# Output: Alder: 25 år

# Eksempel på bruk av print() for å printe ut flere variabler og en streng på en linje:
alder = 25
navn = "Marie"
print(navn, "er", alder, "år gammel")
# Output: Marie er 25 år gammel
```

## Dypdykk

Det finnes også andre måter å bruke `print()` for å få mer nøyaktig debug output. For eksempel kan du bruke en såkalt separator for å skille verdiene du printer ut. Standard separator er mellomrom, men du kan endre denne ved å bruke `sep=`:

```
# Eksempel på bruk av sep= for å endre separator:
print("Mennekser", "Dyr", "Fugler", sep=" | ")
# Output: Mennesker | Dyr | Fugler
```

Du kan også bruke `end=` for å endre hva som skrives ut etter at du har brukt `print()`. Standard end er en linjeskift, men du kan endre denne ved å bruke `end=`:

```
# Eksempel på bruk av end= for å endre end:
dyr = ["hund", "katt", "kanin"]
for dyr in dyr:
    print(dyr, end=" -> ")
# Output: hund -> katt -> kanin ->
```

## Se også

- [Python debugging tutorial](https://realpython.com/python-debugging-pdb/)
- [Effektiv debugging i Python](https://www.datacamp.com/community/tutorials/python-debugging)
- [Debugging med print statements](https://medium.com/@pgburgess/how-to-debug-python-code-using-pretty-print-41f23ffb66d7)
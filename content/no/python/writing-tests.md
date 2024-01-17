---
title:                "Skriving av tester"
html_title:           "Python: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/writing-tests.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Å skrive tester er en viktig del av programmeringsprosessen. Det er en prosess der vi skriver kode for å teste koden vi allerede har skrevet og sikre at den fungerer som den skal. Dette hjelper programvareutviklere med å identifisere og fikse feil i koden sin, og sikrer at den fungerer som forventet.

# Hvordan:

Her er et enkelt eksempel på hvordan man kan skrive tester i Python bruker "assert" uttalelser:

```Python
# Opprett en funksjon som legger sammen to tall
def add(x, y):
    return x + y

# Definer tester for funksjonen vår
assert add(2, 3) == 5  # forventet resultat er 5
assert add(5, -2) == 3 # forventet resultat er 3

print("Alle tester kjørte uten feil, funksjonen fungerer som den skal!")
```

Her har vi skrevet en test for vår "add" funksjon ved hjelp av "assert" uttalelser. Hvis noe ikke fungerer som det skal, vil disse uttalelsene gi en feilmelding og hjelpe oss med å finne feilen.

# Dykk dypere:

I en stadig mer kompleks IT-verden er det viktig å skrive tester for å sikre at koden vår fungerer som den skal. Å ha gode tester i stedet for å debugge koden etter at den allerede er skrevet, sparer oss for mye tid og frustasjon.

Et alternativ til å skrive tester er å bruke en teknikk som kalles "Test Driven Development" (TDD). Dette er en metode der tester skrives før selve koden, og dermed sikrer at koden som blir skrevet oppfyller de ønskede funksjonene.

Du kan også bruke spesielle programmer som "unittest" eller "pytest" for å skrive og kjøre tester i Python. Disse verktøyene tillater deg å skrive mer komplekse tester og kjøre dem automatisk.

# Se også:

- [Hva er tester og hvorfor skal vi skrive dem?](https://www.techopedia.com/definition/27995/test)
- [Test Driven Development - en oversikt](https://agilemanifesto.org/principles.html)
- [Hvordan bruke unittest og pytest i Python](https://realpython.com/python-testing/)
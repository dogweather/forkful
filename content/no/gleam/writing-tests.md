---
title:    "Gleam: Skriving av tester"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/gleam/writing-tests.md"
---

{{< edit_this_page >}}

# Hvorfor

Skal vi teste koden vår? Det er et spørsmål som ofte dukker opp når vi jobber med programmering. Noen ganger kan det virke som en unødvendig prosess, men i virkeligheten spiller testing en viktig rolle i å sikre at koden vår fungerer som den skal. Ved å skrive tester, kan vi oppdage og fikse feil tidligere, noe som sparer oss for tid og frustrasjon senere i utviklingsprosessen.

# Hvordan

Så hvordan skriver vi tester i Gleam? Det er faktisk ganske enkelt! La oss si at vi har en funksjon som legger sammen to tall:

```Gleam
fn add(x, y) {
  x + y
}
```

For å teste denne funksjonen, trenger vi en måte å kalle den og sjekke om returverdien er den vi forventer. Det er her `test`-funksjonen kommer inn i bildet. Vi kan bruke den til å definere en test og gi den en beskrivelse og forventet resultat:

```Gleam
test "add function adds two numbers" {
  expect(add(2, 3)).toBe(5)
}
```

Vi kan også gjøre flere forventede resultater ved å bruke `expect.add`-funksjonen:

```Gleam
test "add function calculates sum correctly" {
  expect.add([
    (add(2, 3), 5),
    (add(10, 5), 15)
  ])
}
```

Vi kan også teste for feil ved å bruke `expect.error`-funksjonen:

```Gleam
test "add function handles unexpected input" {
  expect.error(add("hello", 5))
}
```

Ved å kjøre disse testene, vil vi få en oversikt over hvorvidt funksjonen vår oppfører seg som den skal. Dette gjør det enklere å finne og fikse eventuelle feil.

# Dykk dypere

Å skrive tester er ikke bare nyttig for å kontrollere at koden vår fungerer, det er også en viktig del av å skrive ren og lesbar kode. Ved å skrive tester, tvinger vi oss til å tenke gjennom koden vår og sørge for at den er organisert og fungerer som den skal.

I tillegg er det mulig å skrive mer komplekse tester i Gleam, for eksempel ved å samarbeide med moduler og mocke funksjoner. Ved å utforske disse mulighetene, kan vi styrke vår forståelse av Gleam og hvordan vi kan skrive mer pålitelig kode.

# Se også

- [Offisiell Gleam dokumentasjon for testing](https://gleam.run/book/testing.html)
- [En guide til testdrevet utvikling i Gleam (på engelsk)](https://serokell.io/blog/introduction-to-tdd-in-gleam)
- [Eksempler på hvordan man kan teste Gleam-kode (på engelsk)](https://gist.github.com/davidpelaez/45a254bc0471321d6a6c9ed1faa3d390)
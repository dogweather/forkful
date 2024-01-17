---
title:                "Generering av tilfeldige tall"
html_title:           "Java: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Hva & hvorfor?
Å generere tilfeldige tall er en vanlig oppgave for programmerere. Dette er en måte å inkludere et tilfeldig element i et program eller spill, for å gjøre det mer variert og spennende for brukeren.

# Hvordan:
Å generere tilfeldige tall i Java er enkelt. Du kan bruke Random-klassen, og deretter kalle på metoden nextInt() som tar inn et heltall som grense og returnerer et tilfeldig tall innenfor dette området. For eksempel, hvis du vil ha et tilfeldig tall mellom 1 og 10, kan du bruke koden:

```Java
Random random = new Random();
int randomNumber = random.nextInt(10) + 1;
```

Dette vil generere et tall mellom 1 og 10 og lagre det i variabelen randomNumber.

# Dykk dypere:
Generering av tilfeldige tall har vært en viktig del av programmering siden de tidlige dagene. I begynnelsen ble det brukt på grunn av behovet for å modellere tilfeldige hendelser i vitenskapelige eksperimenter. I dag brukes det mesteparten av tiden i spill og simuleringer, men det er også mange alternative metoder som kan brukes. For eksempel kan du bruke SecureRandom-klassen for å generere kryptografisk sikre tilfeldige tall, eller du kan bruke tilfeldige tall fra en ekstern kilde som et heltallsgrunnlag.

# Se også:
Hvis du er interessert i å lære mer om å generere tilfeldige tall i Java, kan du sjekke ut disse ressursene:

- [Java Random-klassen dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [SecureRandom-klassen dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)
- [GeeksforGeeks guide til å generere tilfeldige tall i Java](https://www.geeksforgeeks.org/generating-random-numbers-in-java/)
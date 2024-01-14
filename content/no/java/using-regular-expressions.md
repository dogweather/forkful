---
title:                "Java: Å bruke regelmessige uttrykk"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Hvorfor

Regular expressions (regex) er et svært kraftig verktøy for databehandling og mønstergjenkjenning i programmering. Ved å bruke regex kan du enkelt finne og manipulere tekst i et bestemt format, for eksempel e-postadresser eller telefonnumre. Dette sparer mye tid og krefter sammenlignet med manuell søking og parsing av tekst.

# Hvordan

For å bruke regular expressions i Java, må du importere java.util.regex-pakken. Her er et enkelt eksempel på hvordan du kan finne et ord i en tekst:

```Java
import java.util.regex.*;

String tekst = "Hei, dette er en tekst med ordet hallo.";

Pattern pattern = Pattern.compile("hallo"); // Her definerer vi mønsteret vi vil finne
Matcher matcher = pattern.matcher(tekst); // Søker etter mønsteret i teksten

if (matcher.find()) { // Hvis mønsteret blir funnet
    System.out.println("Vi fant ordet hallo i teksten!"); // Skriver ut en melding
}
```

Dette vil skrive ut "Vi fant ordet hallo i teksten!" siden teksten inneholder ordet "hallo". Regex'ene er svært fleksible og har flere metoder for å finne og manipulere tekst, som for eksempel å erstatte en del av teksten med et annet mønster eller å ekstrahere deler av teksten basert på et mønster.

# Dypdykk

Regular expressions kan være litt skremmende ved første øyekast på grunn av det store utvalget av metoder og spesielle tegn som kan brukes. Det finnes også mange vanlige feil som kan oppstå når man bruker regex, som å glemme å escape en spesiell karakter. Det er derfor viktig å øve seg og bli kjent med regex før man begynner å bruke det i større prosjekter.

En nyttig ressurs for å lære mer om regular expressions i Java er [Oracle sin dokumentasjon](https://docs.oracle.com/javase/tutorial/essential/regex/index.html). Du kan også finne mange gode øvingsoppgaver og eksempler på nettsteder som [regex101](https://regex101.com/).

# Se også

- [Java API-dokumentasjon for java.util.regex-pakken](https://docs.oracle.com/javase/8/docs/api/java/util/regex/package-summary.html)
- [RegExr - et interaktivt regex-verktøy med mange muligheter](https://regexr.com/)
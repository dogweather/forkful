---
title:                "Java: Nedlasting av en nettside"
simple_title:         "Nedlasting av en nettside"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor
Å laste ned en nettside er en vanlig oppgave for en utvikler, enten det er for å analysere data eller bygge applikasjoner som bruker nettsiden som en kilde. Uansett årsak er det viktig å ha en god forståelse av hvordan man kan laste ned en nettside i Java.

## Hvordan

Vi kan laste ned en nettside i Java ved å bruke klassen URL og åpne en forbindelse til nettsiden. Deretter kan vi lese innholdet på nettsiden med en BufferedReader og skrive det til en lokal fil.

```Java
URL url = new URL("https://www.eksempel.no");
URLConnection connection = url.openConnection();
BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
BufferedWriter writer = new BufferedWriter(new FileWriter("eksempel.html"));
String line;
while ((line = reader.readLine()) != null) {
    writer.append(line);
}
reader.close();
writer.close();
```

Dette eksemplet vil laste ned nettsiden eksempel.no og lagre den som en lokal fil kalt "eksempel.html". Det er viktig å merke seg at dette kun henter den statiske delen av nettsiden, og ikke dynamisk generert innhold.

## Dypdykk

Når du laster ned en nettside, henter du informasjonen som blir sendt tilbake fra en server til din datamaskin. Dette kan være html, javascript, bilder og annet innhold. Det er viktig å merke seg at det er serveren som bestemmer hva som blir sendt tilbake, og hvordan det blir formatert.

Etter å ha lastet ned nettsiden, kan vi utføre forskjellige manipulasjoner på innholdet, for eksempel filtrering av data eller å fjerne unødvendig informasjon. Det er også mulig å bruke forskjellige biblioteker eller rammeverk for å gjøre prosessen mer effektiv og enkel.

## Se også

- [Official Java documentation on URL](https://docs.oracle.com/en/java/javase/12/docs/api/java.base/java/net/URL.html)
- [Java URLConnection tutorial](https://www.baeldung.com/java-http-request)
- [Jsoup - Java HTML parser](https://jsoup.org/)
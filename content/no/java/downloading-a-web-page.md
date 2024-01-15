---
title:                "Nedlasting av en nettside"
html_title:           "Java: Nedlasting av en nettside"
simple_title:         "Nedlasting av en nettside"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor

Skal du lage et program som skal hente en nettside, trenger du å bruke Java. Dette er fordi Java har innebygde biblioteker og funksjoner som gjør det enkelt å laste ned og håndtere nettinnhold.

## Hvordan gjøre det

For å laste ned en nettside i Java, må du først importere biblioteket "java.net.URL" for å få tilgang til funksjonene vi trenger. Deretter kan du bruke følgende kode for å opprette en tilkobling og lese innholdet på nettsiden:

```Java
try {
    URL url = new URL("https://www.example.com"); // bytt ut URLen med den faktiske nettsiden du vil laste ned
    BufferedReader reader = new BufferedReader(new InputStreamReader(url.openStream()));

    String line;
    while ((line = reader.readLine()) != null) {
        System.out.println(line); // skriver ut hver linje i innholdet på nettsiden
    }

    reader.close(); // husk å lukke leseren
} catch (IOException e) {
    e.printStackTrace(); // håndter eventuelle feil som kan oppstå under tilkobling eller lesing
}
```

Det kan også være nyttig å kunne håndtere henting av spesifikke deler av nettsiden, for eksempel bare teksten i en paragraf eller bildet på en knapp. For dette kan du bruke verktøy som "Jsoup" eller "HtmlUnit" for å analysere og manipulere HTML-koden til nettsiden.

## Dykk dypere

Når du henter en nettside, er det viktig å forstå hvordan protokollen "HTTP" fungerer. Dette vil hjelpe deg med å håndtere forskjellige koder for status for tilkoblingen og kunne håndtere eventuelle omdirigeringsløkker eller sikkerhetsproblemer.

Det er også viktig å være forsiktig med hvordan du bruker programmet ditt, slik at du ikke overbelaster nettsiden eller bryter noen opphavsrettslige regler. Sørg for å lese gjennom nettsidens vilkår og betingelser før du laster ned noe innhold.

## Se også

- [Offisiell Java-dokumentasjon for "java.net.URL" biblioteket](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/net/URL.html)
- [Offisiell "Jsoup" dokumentasjon for HTML-analyse i Java](https://jsoup.org/)
- [Offisiell "HtmlUnit" dokumentasjon for manipulering av HTML-kode i Java](https://htmlunit.sourceforge.io/)
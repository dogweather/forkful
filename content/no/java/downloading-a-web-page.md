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

## Hva & Hvorfor?

Det å laste ned en nettside handler om å hente informasjon fra internett og lagre den på vår egen datamaskin eller enhet. Dette gjøres vanligvis av programmere for å kunne behandle og manipulere dataen på en mer effektiv måte.

## Slik gjør du det:

For å laste ned en nettside i Java, kan du bruke klassen URL og metoden openConnection() for å få en tilkobling til nettsiden. Deretter kan du lese dataen ved å bruke en InputStreamReader og BufferedReader. Her er et eksempel på hvordan du kan hente ut kildekoden på en nettside:

```java
URL url = new URL("https://www.example.com");
URLConnection connection = url.openConnection();
InputStreamReader input = new InputStreamReader(connection.getInputStream());
BufferedReader br = new BufferedReader(input);
String line;
while ((line = br.readLine()) != null) {
    System.out.println(line);
}
```

Dette eksempelet vil skrive ut kildekoden til nettsiden i konsollen.

## Dypdykk:

Å laste ned en nettside har vært en viktig funksjonalitet for programmere helt siden internett ble tilgjengelig. Det gir oss muligheten til å hente ut informasjon og bruke den i våre applikasjoner. Det finnes også alternative måter å gjøre dette på, som for eksempel ved hjelp av tredjepartsbiblioteker som Jsoup som gjør det enklere å håndtere HTML-kode.

Når man laster ned en nettside i Java, kreves det at man også behandler eventuelle feil som kan oppstå under tilkoblingen. Det er også viktig å være forsiktig med å laste ned store mengder data, da dette kan føre til at programmet vårt blir tregt og potensielt krasjer.

## Se også:

Her er noen ekstra ressurser om hvordan man kan laste ned en nettside i Java:

- [URL-klassen Java dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/net/URL.html)
- [Lesing av nettside i Java med BufferedReader](https://www.javacodegeeks.com/2012/09/reading-from-url-in-java.html)
- [Jsoup biblioteket](https://jsoup.org/)
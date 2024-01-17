---
title:                "Sende en http-forespørsel"
html_title:           "Java: Sende en http-forespørsel"
simple_title:         "Sende en http-forespørsel"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Sending av HTTP-forespørsler er en vanlig oppgave for programmerere når de ønsker å hente data fra en annen server. HTTP står for HyperText Transfer Protocol og er protokollen som brukes når man kommuniserer med nettsider over internett.

## Slik gjør du det:
For å sende en HTTP-forespørsel i Java kan du bruke klassen HttpURLConnection. Denne klassen er en del av standard Java-pakken og lar deg opprette og sende HTTP-forespørsler enkelt. Et eksempel på hvordan du kan sende en GET-forespørsel til en nettside ser slik ut:

```Java
URL url = new URL("https://www.example.com");
HttpURLConnection con = (HttpURLConnection) url.openConnection();
con.setRequestMethod("GET");

int status = con.getResponseCode();
System.out.println("Statuskode: " + status);

BufferedReader in = new BufferedReader(
    new InputStreamReader(con.getInputStream()));
String inputLine;
StringBuilder response = new StringBuilder();
while ((inputLine = in.readLine()) != null) {
    response.append(inputLine);
}
in.close();

System.out.println("Svar fra server: " + response.toString());
```

Det første du må gjøre er å opprette et URL-objekt som inneholder adressen til nettsiden du ønsker å hente data fra. Deretter oppretter vi en HttpURLConnection som bruker denne URL-en. Vi setter også metoden til å være GET siden vi ønsker å hente informasjon fra nettsiden. Deretter gjør vi et kall til nettsiden og leser svaret fra serveren. Til slutt skriver vi ut statuskoden og svaret fra serveren.

## Dypdykk:
HTTP-protokollen ble utviklet på slutten av 80-tallet og har blitt den mest brukte protokollen for å kommunisere med nettsider over internett. Det finnes også andre måter å sende HTTP-forespørsler på, som for eksempel å bruke tredjeparts biblioteker som Apache HttpComponents eller OkHttp.

En HTTP-forespørsel består av en tittel og en valgfri kropp. Tittelen identifiserer hvilken metode som skal brukes (GET, POST, PUT, osv.) og kroppen kan inneholde data som skal sendes til nettsiden. I Java-koden ovenfor brukte vi GET-metoden, som er den mest vanlige metoden for å hente data fra nettsider.

## Se også:
- [Oracle-beskrivelse av HttpURLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Apache HttpComponents](https://hc.apache.org/httpcomponents-client-4.5.x/index.html)
- [OkHttp](https://square.github.io/okhttp/)
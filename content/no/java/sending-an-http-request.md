---
title:                "Java: Sende en http-forespørsel"
simple_title:         "Sende en http-forespørsel"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Hvorfor

Det å sende HTTP-forespørsler er en essensiell del av webutvikling og applikasjonsprogrammering. Det lar programmerere kommunisere med andre servere og få tilgang til data og tjenester som trengs for å bygge robuste applikasjoner.

# Hvordan

For å sende en HTTP-forespørsel i Java, må du først opprette et objekt av typen `HttpURLConnection` ved å bruke URL for forespørselen. Deretter må du angi hvilken metode som skal brukes, som vanligvis er `GET` eller `POST`. Til slutt må du åpne forbindelsen og lese svaret fra serveren.

```Java
URL url = new URL("https://www.webside.com/api/data");
HttpURLConnection conn = (HttpURLConnection) url.openConnection();
conn.setRequestMethod("GET");
conn.connect();

// Leser svaret fra serveren
InputStreamReader in = new InputStreamReader(conn.getInputStream());
BufferedReader reader = new BufferedReader(in);
String output;
while ((output = reader.readLine()) != null) {
    System.out.println(output);
}
```

Dette vil skrive ut svaret fra serveren i konsollen. Du kan også spesifisere parametere og hodeoverskrifter i forespørselen ved å bruke `conn.setRequestProperty()` og `conn.setDoOutput()` metoder.

# Dykk ned

Når du sender en HTTP-forespørsel, følger du en rekke standarder og protokoller for å sikre at forbindelsen og informasjonen er trygg og pålitelig. Dette inkluderer å angi riktig protokollversjon, autentisering, koding av data og mer.

Det er viktig å forstå disse retningslinjene og følge dem når du sender HTTP-forespørsler for å sikre at applikasjonen din fungerer som forventet og er sikker for brukerne.

# Se også

- [Oracle Java HttpURLConnection API](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [W3C HTTP/1.1-protokollen](https://www.w3.org/Protocols/rfc2616/rfc2616.html)
- [ProgrammableWeb - Hvordan bruke HTTP-forespørsler for å tilgang til et API](https://www.programmableweb.com/news/how-to-use-http-requests-access-web-apis/how-to/2017/08/17)
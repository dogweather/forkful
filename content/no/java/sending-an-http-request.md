---
title:                "Å sende en http-forespørsel"
html_title:           "Java: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Hvorfor

HTTP-forespørsler er en avgjørende del av webutvikling, da de muliggjør kommunikasjon mellom klienter og servere. Ved å lære å sende HTTP-forespørsler i Java, kan du utvikle robuste og pålitelige webapplikasjoner.

# Hvordan

For å sende en HTTP-forespørsel i Java, må du følge disse trinnene:

1. Importer "java.net" pakken i koden din.
2. Opprett en URL-objekt ved å spesifisere URL-en til nettstedet eller API-et du vil sende en forespørsel til.
3. Åpne en forbindelse til URL-en ved å bruke "openConnection ()" metoden.
4. Sett metoden for forespørselen (f.eks. GET, POST, PUT) ved å bruke "setRequestMethod ()" metoden.
5. Legg til eventuelle nødvendige header-felt ved å bruke "setRequestProperty ()" metoden.
6. Hvis du sender en POST- eller PUT-forespørsel, må du angi kroppsdataene til forespørselen ved å bruke "setDoOutput (true)" og skrive dataene til forbindelsen.
7. Kjør forespørselen ved å bruke "getResponseCode ()" metoden, og få responsen ved å bruke "getInputStream ()" metoden.

La oss ta en titt på et eksempel som sender en GET-forespørsel til "https://www.google.com":

```Java
URL url = new URL("https://www.google.com");
HttpURLConnection connection = (HttpURLConnection) url.openConnection();
connection.setRequestMethod("GET");
int responseCode = connection.getResponseCode();
System.out.println("Response Code: " + responseCode);
InputStream inputStream = connection.getInputStream();
```

Dette vil gi følgende utgang:

```
Response Code: 200
```

# Dypdykk

HTTP-forespørsler har forskjellige metoder, som brukes for å utføre forskjellige handlinger. GET-metoden brukes for å hente data fra en bestemt URL, mens POST-metoden brukes for å sende data til en spesifisert URL. Du kan også angi header-felt for å sende ytterligere informasjon til serveren, for eksempel autorisasjonsparametere. Det er også viktig å håndtere feilresponsene ved å sjekke responskoden og ta nødvendige handlinger.

# Se Også

- [Offisiell dokumentasjon for HttpURLConnection](https://docs.oracle.com/javase/10/docs/api/java/net/HttpURLConnection.html)
- [Guide for å håndtere HTTP-forespørsler i Java](https://www.baeldung.com/java-http-request)
- [Enkle HTTP-forespørsler i Java](https://www.codejava.net/java-se/networking/how-to-send-http-request-getpost-in-java)
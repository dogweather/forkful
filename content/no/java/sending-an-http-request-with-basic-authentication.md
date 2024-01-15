---
title:                "Sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Java: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor

Sending av en HTTP-request med grunnleggende autentisering er viktig for å sikre at bare autoriserte brukere har tilgang til sensitive data på nettet. Dette er spesielt viktig i dagens digitale verden hvor personvern og datasikkerhet er av største bekymring.

## Slik gjør du det

```Java
/**
 * Eksempel på en HTTP-request med grunnleggende autentisering.
 * Denne koden vil sende en GET-request til en URL med en brukernavn og et passord.
 * Hvis autentiseringen er vellykket, vil det returnerte responsen bli skrevet ut.
 */
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Base64; // Nødvendig for å enkode brukernavn og passord

public class BasicAuthExample {

	public static void main(String[] args) {

		// URL som skal sendes en HTTP-request til
		String requestUrl = "https://www.example.com/api/data";

		// Brukernavn og passord for autentisering
		String username = "brukernavn";
		String password = "passord";

		try {
			// Oppretter en URL-objekt fra den gitte URL-en
			URL url = new URL(requestUrl);

			// Åpner en HTTP-tilkobling til URL-en
			HttpURLConnection connection = (HttpURLConnection) url.openConnection();

			// Setter metoden til GET
			connection.setRequestMethod("GET");

			// Setter en header for autentisering med Base64-koding 
			String authString = username + ":" + password;
			String authHeaderValue = "Basic " + Base64.getEncoder().encodeToString(authString.getBytes());
			connection.setRequestProperty("Authorization", authHeaderValue);

			// Leser inn responsen fra tilkoblingen og lagrer den i en BufferedReader
			BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(connection.getInputStream()));

			// Leser responsen linje for linje og skriver den ut
			String inputLine;
			StringBuilder response = new StringBuilder();
			while ((inputLine = bufferedReader.readLine()) != null) {
				response.append(inputLine);
			}
			bufferedReader.close();

			// Skriver ut responsen
			System.out.println(response.toString());

		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
```

### Eksempel output:

```
{"status": "Success", "data": [{...}, {...}, {...}]} 
```

## Dypdykk

Grunnleggende autentisering er en enkel og effektiv metode for å autentisere klienter som vil ha tilgang til beskyttede ressurser på nettet. Denne metoden krever at klienten sender brukernavn og passord i en HTTP-header, kalt "Authorization". Serveren vil deretter verifisere brukerens identitet og gi tilgang til de nødvendige ressursene hvis autentiseringen er vellykket. Det er viktig å merke seg at det er viktig å bruke HTTPS i stedet for HTTP når man sender autentiseringsinformasjon for å sikre at dataene ikke blir avlyttet.

## Se også

- [HTTP Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
- [Java URL Connection](https://www.baeldung.com/java-http-url-connection)
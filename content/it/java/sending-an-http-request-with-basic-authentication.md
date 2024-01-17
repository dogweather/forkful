---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Java: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Cosa e Perché?

Invio delle richieste HTTP con autenticazione di base è il processo di invio di una richiesta web a un server utilizzando le credenziali di accesso di base. I programmatori lo fanno per accedere a risorse protette su un server web, come file o dati.

## Come fare:

```Java
// Importare la classe HTTPURLConnection dalla libreria Java
import java.net.HttpURLConnection;

// Dichiarare le credenziali di accesso
String username = "user";
String password = "password";

// Creare un'istanza della classe URL con l'URL desiderato
URL url = new URL("https://www.esempio.com/resource");

// Creare una connessione HTTP con l'URL
HttpURLConnection connection = (HttpURLConnection) url.openConnection();

// Impostare il metodo HTTP su GET
connection.setRequestMethod("GET");

// Aggiungere le credenziali di accesso alla richiesta
String authString = username + ":" + password;
byte[] authEncBytes = Base64.getEncoder().encode(authString.getBytes());
String authStringEnc = new String(authEncBytes);
connection.setRequestProperty("Authorization", "Basic " + authStringEnc);

// Leggere la risposta dal server
BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
String inputLine;
StringBuilder response = new StringBuilder();
while ((inputLine = in.readLine()) != null) {
    response.append(inputLine);
}
in.close();

// Stampare la risposta
System.out.println(response.toString());
```

L'output sarà il contenuto della risorsa protetta.

## Approfondimenti

L'autenticazione di base è uno dei metodi di autenticazione più semplici e meno sicuri. È stato definito nel 1996 nella specifica RFC 2617 e consiste nell'aggiungere un'intestazione di autorizzazione contenente il nome utente e la password codificati in Base64 alla richiesta HTTP. Questo metodo non è consigliato per l'invio di informazioni sensibili come le credenziali di accesso.

Un'alternativa più sicura all'autenticazione di base è l'autenticazione digest, che utilizza una crittografia più forte per proteggere le credenziali di accesso.

L'implementazione dell'autenticazione di base in Java utilizza le classi HttpURLConnection e Base64 della libreria Java. È possibile personalizzare ulteriormente l'autenticazione di base utilizzando altre librerie o framework come Apache HttpClient.

## Vedi anche

- Documentazione ufficiale di Java per HttpURLConnection: https://docs.oracle.com/javase/7/docs/api/java/net/HttpURLConnection.html
- Specifica RFC 2617: https://tools.ietf.org/html/rfc2617
- Overview dell'autenticazione di base: https://www.httpwatch.com/httpgallery/authentication/
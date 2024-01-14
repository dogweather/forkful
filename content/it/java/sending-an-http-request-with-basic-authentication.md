---
title:                "Java: Inviare una richiesta http con l'autenticazione di base"
simple_title:         "Inviare una richiesta http con l'autenticazione di base"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché
*Perché dovresti inviare una richiesta HTTP con autenticazione di base?*

Quando si desidera accedere a un server protetto da autenticazione di base, è necessario inviare una richiesta HTTP con le credenziali dell'utente per ottenere l'accesso. Questo è particolarmente utile per creare applicazioni che richiedono una protezione aggiuntiva dei dati sensibili.

## Come fare
Ecco un esempio di codice Java per creare una richiesta HTTP con autenticazione di base:

```Java
// Importa le librerie necessarie
import java.net.Authenticator;
import java.net.PasswordAuthentication;
import java.net.URL;
import java.net.HttpURLConnection;
import java.io.BufferedReader;
import java.io.InputStreamReader;

public class BasicAuthExample {
    // Definisce URL, nome utente e password
    private static final String URL = "https://example.com";
    private static final String USERNAME = "utente";
    private static final String PASSWORD = "password";

    public static void main(String[] args) throws Exception {
        // Imposta l'autenticazione di base con le credenziali specificate
        Authenticator.setDefault(new Authenticator() {
            protected PasswordAuthentication getPasswordAuthentication() {
                return new PasswordAuthentication(USERNAME, PASSWORD.toCharArray());
            }
        });

        // Crea una nuova connessione HTTP
        URL url = new URL(URL);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();

        // Imposta il metodo HTTP e l'intestazione di autenticazione
        conn.setRequestMethod("GET");
        conn.setRequestProperty("Authorization", "Basic " + java.util.Base64.getEncoder().encodeToString((USERNAME + ":" + PASSWORD).getBytes()));

        // Legge la risposta dal server
        BufferedReader br = new BufferedReader(new InputStreamReader(conn.getInputStream()));
        String output;
        while ((output = br.readLine()) != null) {
            System.out.println(output);
        }

        // Chiude la connessione
        conn.disconnect();
    }
}
```

Esempio di output della richiesta HTTP con autenticazione di base:

```Java
HTTP/1.1 200 OK
Date: Mon, 15 Mar 2021 12:00:00 GMT
Content-Type: text/html; charset=UTF-8

<!DOCTYPE html>
<html>
  <head>
    <title>Example Domain</title>
  </head>
  <body>
    <h1>Accesso riuscito!</h1>
  </body>
</html>
```

## Approfondimento
Per inviare una richiesta HTTP con autenticazione di base, è necessario prima comprendere come funziona l'autenticazione di base. Questo tipo di autenticazione è un meccanismo di sicurezza molto semplice che richiede l'invio delle credenziali dell'utente in chiaro al server. Questo è un metodo molto comune per proteggere le risorse web private e sensibili.

Quando si invia una richiesta HTTP con autenticazione di base, il nome utente e la password vengono codificati in base64 e aggiunti all'intestazione di autenticazione della richiesta. Il server decodificherà poi le credenziali e verificherà se corrispondono a quelle registrate per l'utente. Se le credenziali sono corrette, l'utente otterrà l'accesso alla risorsa richiesta.

## Vedi anche
- [Tutorial Java su come inviare una richiesta HTTP](https://www.baeldung.com/java-http-request)
- [Documentazione ufficiale di Java per la classe HttpURLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Spiegazione dettagliata sull'autenticazione di base](https://developer.mozilla.org/it/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
---
title:                "Invio di una richiesta http con autenticazione di base"
html_title:           "Java: Invio di una richiesta http con autenticazione di base"
simple_title:         "Invio di una richiesta http con autenticazione di base"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché

In un mondo in cui la sicurezza dei dati è sempre più importante, l'autenticazione di base è uno dei modi più sicuri per inviare richieste HTTP. Questo metodo di autenticazione garantisce che solo i destinatari autorizzati possano accedere ai dati.

## Come fare

Per inviare una richiesta HTTP con l'autenticazione di base in Java, è necessario seguire questi semplici passaggi:

1. Importare la classe "HttpURLConnection" dal pacchetto "java.net".
2. Creare un'istanza di "URL" passando l'URL della richiesta come parametro.
3. Chiamare il metodo "openConnection()" sull'istanza URL per ottenere un'istanza di "HttpURLConnection".
4. Impostare il metodo della richiesta (GET, POST, PUT, DELETE) utilizzando il metodo "setRequestMethod()".
5. Aggiungere le credenziali di autenticazione utilizzando il metodo "setRequestProperty()" e passando l'header "Authorization" con il valore "Basic " seguito da username e password codificati in Base64.
6. Inviare la richiesta utilizzando il metodo "getInputStream()" e leggere la risposta utilizzando un oggetto "BufferedReader".

Ecco un esempio di codice Java che esegue una richiesta GET con autenticazione di base e stampa la risposta ottenuta:

```java
import java.net.*;
import java.io.*;

public class BasicAuthRequest {
    public static void main(String[] args) throws Exception {
        URL url = new URL("https://www.example.com");
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();
        connection.setRequestMethod("GET");
        String encodedAuth = Base64.getEncoder().encodeToString(("username:password").getBytes());
        connection.setRequestProperty("Authorization", "Basic " + encodedAuth);
        
        BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
        String input;
        StringBuffer response = new StringBuffer();
        
        while ((input = in.readLine()) != null) {
            response.append(input);
        }
        in.close();
        
        System.out.println(response.toString());
    }
}
```

Eseguendo questo codice, si otterrà una risposta HTTP con codice di stato 200 e il corpo della risposta verrà stampato a console.

## Approfondimento

È importante notare che l'autenticazione di base non è il metodo più sicuro per inviare richieste HTTP, poiché le credenziali sono codificate ma non criptate. Tuttavia, è comunque una buona pratica utilizzare questo metodo insieme ad altre misure di sicurezza per garantire la protezione dei dati.

Poiché l'autenticazione di base richiede che le credenziali siano codificate in Base64, è importante utilizzare librerie come "java.util.Base64" per assicurarsi che la codifica sia fatta correttamente.

## Vedi anche

- Tutorial di Oracle su come inviare richieste HTTP in Java: https://www.oracle.com/technical-resources/articles/javase/submit-data-ssl-certificates.html 
- Documentazione ufficiale su "java.net.HttpURLConnection": https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html 
- Documentazione ufficiale su "java.util.Base64": https://docs.oracle.com/javase/8/docs/api/java/util/Base64.html
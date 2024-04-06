---
date: 2024-01-20 18:02:15.519500-07:00
description: "Come Fare: L'autenticazione di base HTTP risale agli albori del web\
  \ e si trova nella specifica HTTP 1.0 del 1996. Nonostante la sua semplicit\xE0\
  , non \xE8 la\u2026"
lastmod: '2024-04-05T21:53:44.079972-06:00'
model: gpt-4-1106-preview
summary: L'autenticazione di base HTTP risale agli albori del web e si trova nella
  specifica HTTP 1.0 del 1996.
title: Inviare una richiesta http con autenticazione di base
weight: 45
---

## Come Fare:
```java
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Base64;

public class BasicAuthExample {
    public static void main(String[] args) {
        try {
            URL url = new URL("http://example.com/api/data");
            String userCredentials = "username:password";
            String basicAuth = "Basic " + new String(Base64.getEncoder().encode(userCredentials.getBytes()));
            
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("GET");
            connection.setRequestProperty("Authorization", basicAuth);
            
            int responseCode = connection.getResponseCode();
            System.out.println("Response Code: " + responseCode);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Output:
```
Response Code: 200
```

## Analisi Approfondita:
L'autenticazione di base HTTP risale agli albori del web e si trova nella specifica HTTP 1.0 del 1996. Nonostante la sua semplicità, non è la scelta più sicura perché le credenziali vengono inviate in chiaro, codificate in Base64, che è facilmente decodificabile. Pertanto, si raccomanda di usarla solamente con HTTPS, che cripta il traffico.

Le alternative includono OAuth e JWT (JSON Web Tokens), che offrono meccanismi di autenticazione più avanzati e sicuri.

Per l'esecuzione di una richiesta HTTP con autenticazione di base in Java, è cruciale creare una stringa di autenticazione in Base64, che verrà aggiunta nell'header della richiesta. In Java 11 e versioni successive, si potrebbe usare anche `HttpClient` invece di `HttpURLConnection` per una approccio più moderno e funzionale.

## Vedi Anche:
- [La specifica del Basic Access Authentication](https://tools.ietf.org/html/rfc7617)
- [Guida Oracle alla Basic Authentication con HttpURLConnection](https://docs.oracle.com/javase/tutorial/networking/urls/readingWriting.html)
- [Approccio più moderno con HttpClient — Java 11](https://openjdk.java.net/groups/net/httpclient/intro.html)

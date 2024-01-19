---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Bash: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
L'invio di una richiesta HTTP con autenticazione di base è un metodo per fornire nome utente e password in una richiesta HTTP. I programmatori la utilizzano per accedere alle risorse web protette che richiedono credenziali di login.

## Come Fare:
Ecco un semplice esempio di come inviare una richiesta HTTP con autenticazione di base in Java utilizzando la libreria `java.net.HttpURLConnection`.

```Java
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Base64;

public class Main {
    public static void main(String[] args) throws Exception {
        String urlStr = "http://miosito.com";
        String username = "mio_nome_utente";
        String password = "mia_password";

        URL url = new URL(urlStr);
        String credentials = username + ":" + password;
        String basicAuth = "Basic " + new String(Base64.getEncoder().encode(credentials.getBytes()));

        HttpURLConnection connection = (HttpURLConnection) url.openConnection();
        connection.setRequestProperty("Authorization", basicAuth);

        int responseCode = connection.getResponseCode();
        System.out.println("Risposta: " + responseCode);
    }
}
```

Quando eseguiamo il programma, dovremmo ottenere qualcosa di simile a questo:

```
Risposta: 200
```

## Approfondimenti
L'autenticazione HTTP di base è un sistema di sicurezza standard da molto tempo, risalente alla definizione della specifica HTTP/1.0 nel 1996. Nonostante sia una tecnica piuttosto vecchia, è ancora ampiamente utilizzata per la sua semplicità.

Tuttavia, ha importanti svantaggi. Ad esempio, le credenziali non sono crittografate, ma semplicemente codificate in Base64, e quindi potrebbero essere facilmente rivelate. Inoltre, la gestione delle sessioni è lasciata al server, che può portare a inefficienze.

Ci sono molte alternative a HTTP Basic Auth, come OAuth, che offrono maggiore sicurezza e altre funzionalità. Inoltre,
puoi considerare l'uso delle nuove API `java.net.http` presenti in Java 11+, che forniscono una più moderna e flessibile interfaccia HTTP client.

## Vedi Anche
1. [Documentazione ufficiale di HttpURLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
2. [Guida all'autenticazione HTTP](https://developer.mozilla.org/it/docs/Web/HTTP/Authentication)
3. [Documentazione ufficiale di java.net.http](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/package-summary.html)
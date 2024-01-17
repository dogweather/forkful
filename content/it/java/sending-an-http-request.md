---
title:                "Inviare una richiesta http"
html_title:           "Java: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
L'invio di una richiesta HTTP è un concetto importante nella programmazione Java. Si tratta di una comunicazione tra il tuo programma e un altro server o risorsa web. I programmatori utilizzano questa tecnologia per scambiare dati e informazioni tra diverse fonti.

## Come Fare:
Per inviare una richiesta HTTP in Java, è necessario utilizzare la classe java.net.HttpURLConnection. Questa classe fornisce metodi per impostare l'URL di destinazione, il metodo di richiesta e gestire le eventuali risposte. Ecco un esempio di codice:

```java
import java.net.HttpURLConnection;
import java.net.URL;

public class HTTPRequestExample{
    public static void main(String[] args){
        try{
            //Imposta l'URL di destinazione
            URL url = new URL("https://www.example.com");
            
            //Crea una nuova istanza di HttpURLConnection
            HttpURLConnection con = (HttpURLConnection) url.openConnection();
            
            //Imposta il metodo di richiesta
            con.setRequestMethod("GET");
            
            //Ottieni la risposta dalla richiesta
            int responseCode = con.getResponseCode();
            
            //Stampa la risposta
            System.out.println("Codice di risposta: " + responseCode);
            
            //Chiudi la connessione
            con.disconnect();
        } catch(Exception e){
            System.out.println("Errore durante la richiesta: " + e);
        }
    }
}

```

Output:

Codice di risposta: 200

## Deep Dive:
L'invio di richieste HTTP è una tecnologia fondamentale nella comunicazione web. È stato introdotto per la prima volta nel 1991 e da allora è diventato uno standard per lo scambio di dati tra diversi sistemi. Ci sono anche altre opzioni per inviare richieste HTTP in Java, come la libreria Apache HttpClient.

## Vedi Anche:
Ulteriori informazioni su come inviare richieste HTTP in Java possono essere trovate su questi siti:
- Documentazione ufficiale Java: https://docs.oracle.com/javase/tutorial/networking/urls/readingWriting.html
- Tutorial su Come Fare: https://www.baeldung.com/java-http-request
- Apache HttpClient: https://hc.apache.org/httpcomponents-client-ga/index.html
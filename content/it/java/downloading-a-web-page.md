---
title:                "Scaricare una pagina web"
html_title:           "Java: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché
Molti di noi sono spesso curiosi di conoscere il codice sorgente di una pagina web o di voler salvare una copia della pagina per la lettura offline. Fortunatamente, Java ci offre un modo semplice e veloce per scaricare una pagina web direttamente nel nostro codice.

## Come Fare
Per scaricare una pagina web in Java, possiamo utilizzare la classe HttpURLConnection e il suo metodo connect(). Ecco un esempio di codice che ci mostra come scaricare il codice sorgente di una pagina web e stamparlo in output:

```Java
import java.net.HttpURLConnection;
import java.net.URL;
import java.io.BufferedReader;
import java.io.InputStreamReader;

public class DownloadWebPage {
    public static void main(String[] args) {
        try {
            // specificare l'URL della pagina web da scaricare
            URL url = new URL("https://www.example.com");
            // aprire la connessione
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            // impostare il metodo di richiesta come GET
            connection.setRequestMethod("GET");
            // leggere il codice sorgente della pagina
            BufferedReader input = new BufferedReader(new InputStreamReader(connection.getInputStream()));
            // stampare il codice sorgente
            String inputLine;
            while ((inputLine = input.readLine()) != null) {
                System.out.println(inputLine);
            }
            // chiudere la connessione
            input.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

L'output di questo codice sarà il codice sorgente della pagina web scaricata. Invece di stamparlo, possiamo anche salvare questo codice in un file utilizzando le normali operazioni di I/O in Java.

## Deep Dive
Ora che abbiamo visto come scaricare una pagina web in Java, possiamo approfondire ulteriormente questo processo. Possiamo utilizzare il metodo connect() della classe HttpURLConnection per configurare la nostra richiesta in modo più specifico. Ad esempio, possiamo impostare un timeout o aggiungere parametri alla nostra richiesta.

Inoltre, possiamo anche utilizzare altre classi Java come URLConnection o HttpClient per scaricare una pagina web.

## Vedi Anche
- [Classe HttpURLConnection di Oracle JavaDocs](https://docs.oracle.com/javase/7/docs/api/java/net/HttpURLConnection.html)
- [Esempi di download di pagine web in Java](https://www.baeldung.com/java-download-webpage)
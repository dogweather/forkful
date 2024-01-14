---
title:                "Java: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché

Scaricare una pagina web è un'attività essenziale per molti programmatori Java. Ciò consente loro di accedere ai dati da una pagina web e utilizzarli all'interno delle loro applicazioni. In questo post, esploreremo come eseguire questa operazione utilizzando Java.

## Come Fare

Per scaricare una pagina web in Java, è necessario utilizzare la classe `URL` e il metodo `openStream()`. Ecco un esempio di codice che mostra come farlo:

```
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;

public class WebPageDownloader {
    public static void main(String[] args) throws IOException {
        URL url = new URL("https://www.example.com");  // sostituisci l'URL con quello desiderato
        InputStream inputStream = url.openStream();
        BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));

        String line;
        while ((line = reader.readLine()) != null) {
            System.out.println(line);
        }

        reader.close();
    }
}
```

Questo codice utilizzerà il metodo `openStream()` per aprire un'istanza della classe `InputStream` dalla pagina web desiderata. Successivamente, creiamo un'istanza della classe `BufferedReader` che ci consentirà di leggere i dati dalla pagina. Infine, stampiamo i dati utilizzando un ciclo while che legge ogni riga del buffer e la stampa a schermo.

L'output di questo esempio sarà l'intera pagina web, stampata riga per riga.

```
<!DOCTYPE html>
<html>
<head>
    <title>Example Domain</title>
    
    <meta charset="utf-8" />
    <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <style type="text/css">
    body {
        background-color: #f0f0f2;
        margin: 0;
        padding: 0;
        font-family: "Open Sans", "Helvetica Neue", Helvetica, Arial, sans-serif;
        
    }
    div {
        width: 600px;
        margin: 5em auto;
        padding: 50px;
        background-color: #fff;
        border-radius: 1em;
    }
    a:link, a:visited {
        color: #38488f;
        text-decoration: none;
    }
    @media (max-width: 700px) {
        body {
            background-color: #fff;
        }
        div {
            width: auto;
            margin: 0 auto;
            border-radius: 0;
            padding: 1em;
        }
    }
    </style>    
</head>

<body>
<div>
    <h1>Example Domain</h1>
    <p>This domain is for use in illustrative examples in documents. You may use this
    domain in literature without prior coordination or asking for permission.</p>
    <p><a href="https://www.iana.org/domains/example">More information...</a></p>
</div>
</body>
</html>
```

## Approfondimento

Oltre al metodo `openStream()`, esistono anche altri modi per scaricare una pagina web in Java. Ad esempio, è possibile utilizzare la libreria Apache HttpComponents per creare una connessione e scaricare i dati. Inoltre, è importante gestire correttamente le eccezioni e i possibili errori che possono verificarsi durante il processo di download.

Inoltre, è possibile utilizzare i metodi della classe `URL` come `openConnection()` e `getContent()` per ottenere informazioni aggiuntive sulla connessione e sull'output della pagina web.

In ogni caso, è importante leggere la documentazione e comprendere il processo per evitare possibili errori o problemi.

## Vedi Anche

- Documentazione ufficiale di Java sulla classe URL: https://docs.oracle.com/javase/7/docs/api/java/net/URL.html
- Tutorial su Java per scaricare una pagina web: https://www.baeldung.com/java-download-webpage
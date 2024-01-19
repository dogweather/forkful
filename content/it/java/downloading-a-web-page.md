---
title:                "Scaricare una pagina web"
html_title:           "C++: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Scaricare una pagina web significa recuperare i dati HTML di una pagina da un server web. I programmatori spesso lo fanno per l'elaborazione dei dati, come per l'estrazione delle informazioni rilevanti, o per testare la funzionalità del sito web.

## Come fare:

Ecco un esempio di codice Java che mostra come scaricare una pagina web utilizzando la classe `java.net.URL` e `java.net.URLConnection`.

```java
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;

public class Downloader {
    public static void main(String[] args) throws Exception {
        URL url = new URL("http://www.example.com");
        URLConnection connection = url.openConnection();
        BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
        String line;
        while ((line = reader.readLine()) != null) {
            System.out.println(line);
        }
        reader.close();
    }
}
```
L'output sarà l'HTML della pagina www.example.com stampato nella console.

## Approfondimenti

Storicamente, i programmatori hanno utilizzato varie librerie e metodi per scaricare pagine web. Alcuni utilizzano librerie come `Jsoup` che forniscono più funzionalità come il parsing del DOM o Javascript rendering.

Alcuni linguaggi come Python hanno semplici funzionalità integrate, come `requests`, per scaricare pagine web. Tuttavia, in Java, si possono utilizzare classi incorporate come `java.net.URL` e `java.net.URLConnection` per aimplementare questa funzione, come dimostrato sopra.

Alcune implementazioni utilizzano approcci asincroni per scaricare pagine web; questi sono spesso utili per scaricare grandi quantità di pagine web in un tempo relativamente breve.

## Vedi Anche

Per un approfondimento sul parsing del DOM e Javascript rendering in Java, consulta la documentazione di Jsoup (https://jsoup.org/).

Per una comparazione dei metodi di scaricamento delle pagine web in diversi linguaggi di programmazione, consulta questo link (https://www.toptal.com/developers/web-scraping-technologies-guide).

Per un'analisi più profonda delle classi `java.net.URL` e `java.net.URLConnection`, riferisci alla documentazione ufficiale Java (https://docs.oracle.com/javase/8/docs/api/java/net/URLConnection.html).
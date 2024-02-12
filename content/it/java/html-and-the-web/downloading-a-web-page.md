---
title:                "Scaricare una pagina web"
aliases:
- /it/java/downloading-a-web-page.md
date:                  2024-01-20T17:44:19.238569-07:00
model:                 gpt-4-1106-preview
simple_title:         "Scaricare una pagina web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Scaricare una pagina web significa prelevare il contenuto HTML di una pagina da Internet. I programmatori fanno questo per processare dati, alimentare applicazioni con contenuti dinamici o analizzare siti web.

## Come fare:
Per scaricare una pagina web in Java, possiamo usare la classe `URLConnection` combinata con un `BufferedReader`. Ecco un esempio pratico:

```Java
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;

public class WebPageDownloader {

    public static void main(String[] args) {
        try {
            URL url = new URL("http://example.com");
            URLConnection connection = url.openConnection();
            BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
            
            String inputLine;
            while ((inputLine = reader.readLine()) != null) {
                System.out.println(inputLine);
            }
            reader.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Output di esempio (dipenderà dalla pagina che scarichi):
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</html>
```

## Approfondimenti:
Scaricare una pagina web non è una novità; si fa dai tempi delle connessioni dial-up. Prima si usavano librerie come Apache HttpClient. Ora, con Java 11 o superiore, c'è `HttpClient`, che semplifica il processo e gestisce meglio le connessioni.

L'approccio mostrato sopra usa classi vecchie ma affidabili che funzionano dalla JDK 1.0. Se usi Java 11 o superiore, considera `HttpClient`. La libreria esterna Jsoup è utile per parsing di HTML. Remeber, sempre verifica la legalità dello scraping e l'uso di dati scaricati.

## Vedi anche:
- Documentazione Oracle su `URLConnection`: https://docs.oracle.com/javase/8/docs/api/java/net/URLConnection.html
- HttpClient (Java 11+): https://openjdk.java.net/groups/net/httpclient/intro.html
- Jsoup, per parsing HTML: https://jsoup.org/

Tieni presente che queste informazioni sono aggiornate alla data del mio ultimo aggiornamento nel 2023. Verifica sempre la documentazione più recente per gli aggiornamenti delle API.

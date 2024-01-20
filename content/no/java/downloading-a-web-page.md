---
title:                "Laste ned en nettside"
html_title:           "Elixir: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å laste ned en nettside betyr å hente data (som HTML, CSS, bilder) fra en server til din egen datamaskin. Programmerere gjør dette for å analysere data, utføre webskraperi, eller skape en offline versjon for senere bruk.

## Hvordan

I Java kan vi bruke `java.net.HttpURLConnection` for å laste ned en nettside. La oss se på en grunnleggende kodeblokk:

```Java
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;

public class Main {
    public static void main(String[] args) throws Exception {
        String urlStr = "http://www.example.com";
        URL url = new URL(urlStr);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();

        // Request setup
        conn.setRequestMethod("GET");
        conn.setConnectTimeout(5000);
        
        BufferedReader reader = new BufferedReader(new InputStreamReader(conn.getInputStream()));
        String line;
		
        // Reading lines of the page
        while ((line = reader.readLine()) != null) {
            System.out.println(line);
        }
        reader.close();
    }
}
```

Når du kjører dette, bør du se HTML-koden til "www.example.com" i konsollen.

## Deep Dive

Da Internett var i sin spede begynnelse, var nedlasting av en nettside like enkelt som å hente en fil fra en server. Med økende kompleksitet av nettsteder og bruken av dynamisk innhold, er prosessen mer kompleks. 

Alternativene til `HttpURLConnection` inkluderer `java.net.URL`, `org.apache.http.client.methods.HttpGet` (fra Apache HttpClient library), og Jsoup (en tredjepartsbibliotek spesialisert for webscraping).

Valget av hvilken tilnærming å bruke avhenger av behovene dine. Hvis du skal skrape store mengder data fra en nettside, kan det være verdt å vurdere den mer spesialiserte Jsoup.

Implmenteringsdetaljer involverer hvordan du behandler forskjellige typer media på nettsiden (ikke bare tekst), og hvordan håndtere error codes (som 404 Not Found eller 500 Internal Server Error).

## Se også

- [Java HttpURLConnection documentation](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/net/HttpURLConnection.html)
- [Apache HttpClient library](https://hc.apache.org/httpcomponents-client-4.5.x/quickstart.html)
- [Jsoup documentation](https://jsoup.org/)
  
Undersøk for mer eksplisitt informasjon og flere kjøre eksempler.
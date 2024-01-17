---
title:                "Skicka ett http-anrop"
html_title:           "Java: Skicka ett http-anrop"
simple_title:         "Skicka ett http-anrop"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skicka en HTTP-begäran är en vanlig uppgift för programmerare. Det är en metod för att kommunicera med servrar och få återkoppling på våra begäranden. Genom att skicka en HTTP-begäran kan vi till exempel hämta information från en webbsida, skicka data till en databas eller få tillgång till ett API.

## Så här gör du:

För att skicka en HTTP-begäran i Java använder vi oss av klassen "HttpURLConnection" i "java.net" biblioteket. Vi måste ange URL:en för servern vi vill kommunicera med och ange vilken typ av begäran vi vill göra (t.ex. GET, POST, PUT). Här är ett exempel på kod som skickar en GET-begäran och får tillbaka ett svar från servern:

```Java
try {
    // Ange URL:en för servern
    URL url = new URL("https://example.com/api/data");
    
    // Skapa en HTTP-anslutning
    HttpURLConnection connection = (HttpURLConnection) url.openConnection();
    
    // Ange vilken typ av begäran vi vill göra
    connection.setRequestMethod("GET");
    
    // Hämta svaret från servern
    BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
    
    // Läs svaret rad för rad
    String inputLine;
    StringBuilder response = new StringBuilder();
    while ((inputLine = in.readLine()) != null) {
        response.append(inputLine);
    }
    in.close();
    
    // Skriv ut svaret
    System.out.println(response.toString());
} catch (IOException e) {
    System.out.println("Något gick fel med begäran: " + e.getMessage());
}
```

Körning av kodexemplet ovan kommer att skriva ut det svar som servern skickade tillbaka.

## Djupdykning:

HTTP-protokollet utvecklades av Tim Berners-Lee och presenterades för första gången 1991. Det finns många andra sätt att kommunicera med servrar, såsom FTP, SMTP och WebSocket. Men HTTP är det vanligaste protokollet för att implementera API:er och för att hämta och skicka data över internet.

Om du vill skicka mer komplexa begäranden som innehåller till exempel inloggning eller autentisering, kan det vara lämpligt att använda ett tredjepartsbibliotek som "OkHttp" eller "Apache HttpComponents".

## Se även:

- [Java HttpURLConnection documentation](https://docs.oracle.com/javase/9/docs/api/java/net/HttpURLConnection.html)
- [OkHttp](https://square.github.io/okhttp/)
- [Apache HttpComponents](https://hc.apache.org/index.html)
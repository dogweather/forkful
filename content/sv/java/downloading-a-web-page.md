---
title:                "Java: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför
Att ladda ner en webbsida kan vara användbart för många ändamål, såsom att analysera data, söka information eller skapa en egen offlineversion av en webbplats.

## Hur man
För att ladda ner en webbsida i Java kan man använda sig av Java-biblioteket "URLConnection". Först måste man skapa en ny URL-objekt med adressen till webbsidan som parameter. Sedan kan man öppna en anslutning till webbadressen med "openConnection()" metoden. För att hämta webbsidans innehåll i form av en "InputStream" behöver man sedan använda "getInputStream()" funktionen. Slutligen kan man läsa den hämtade datan och spara den i en fil med hjälp av "FileOutputStream".

```Java
import java.io.*;
import java.net.*;

public class WebPageDownloader {
    public static void main(String[] args) {
        try {
            // skapar en ny URL-objekt med adressen till webbsidan
            URL url = new URL("https://www.example.com/");

            // öppnar en anslutning till webbadressen
            URLConnection connection = url.openConnection();

            // hämtar webbsidans innehåll i form av en InputStream
            InputStream inputStream = connection.getInputStream();

            // läser och sparar datan i en output-fil
            FileOutputStream outputStream = new FileOutputStream("example.html");
            int bytesRead = -1;
            byte[] buffer = new byte[1024];
            while ((bytesRead = inputStream.read(buffer)) != -1) {
                outputStream.write(buffer, 0, bytesRead);
            }

            // stänger strömmarna
            inputStream.close();
            outputStream.close();

            System.out.println("Webbsidan laddades ner framgångsrikt.");
        } catch (MalformedURLException e) {
            System.out.println("Ogiltig URL.");
        } catch (IOException e) {
            System.out.println("Något gick fel vid nedladdningen av webbsidan.");
        }
    }
}
```

**Output:**
Webbsidan laddades ner framgångsrikt. Datan sparas i filen "example.html".

## Djupdykning
Att ladda ner en webbsida är inte alltid enkelt. Det finns många faktorer som kan påverka hur sidan visas, såsom autentisering, cookies och JavaScript-kod. För att säkerställa att hela sidan laddas ner korrekt kan man behöva använda sig av ytterligare Java-bibliotek, såsom "HttpURLConnection" eller "Jsoup", beroende på sidans komplexitet.

## Se även
- [Java-biblioteket URLConnection](https://docs.oracle.com/javase/7/docs/api/java/net/URLConnection.html)
- [HttpURLConnection för mer avancerad hantering av HTTP-anrop](https://docs.oracle.com/javase/7/docs/api/java/net/HttpURLConnection.html)
- [Jsoup för HTML-manipulering i Java](https://jsoup.org/)
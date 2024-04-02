---
date: 2024-01-20 17:44:16.977159-07:00
description: "Att ladda ner en webbsida inneb\xE4r att h\xE4mta HTML-koden som browsern\
  \ anv\xE4nder f\xF6r att visa sidan. Programmerare g\xF6r detta f\xF6r att skrapa\
  \ data, automatiskt\u2026"
lastmod: '2024-03-13T22:44:37.786268-06:00'
model: gpt-4-1106-preview
summary: "Att ladda ner en webbsida inneb\xE4r att h\xE4mta HTML-koden som browsern\
  \ anv\xE4nder f\xF6r att visa sidan. Programmerare g\xF6r detta f\xF6r att skrapa\
  \ data, automatiskt\u2026"
title: "H\xE4mta en webbsida"
weight: 42
---

## Vad & Varför?
Att ladda ner en webbsida innebär att hämta HTML-koden som browsern använder för att visa sidan. Programmerare gör detta för att skrapa data, automatiskt testa webbplatser eller övervaka förändringar.

## Hur gör man?:
```java
import java.io.*;
import java.net.*;

public class WebPageDownloader {
    public static void main(String[] args) throws IOException {
        URL url = new URL("http://example.com");
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();
        
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()))) {
            String inputLine;
            StringBuilder content = new StringBuilder();
            
            while ((inputLine = reader.readLine()) != null) {
                content.append(inputLine).append("\n");
            }
            
            System.out.println(content.toString());
        } finally {
            connection.disconnect();
        }
    }
}
```
_Sample output:_
HTML-innehållet i http://example.com, utskrivet i konsolen.

## Fördjupning:
För 20-ish år sedan använde Java-klasser som `URLConnection`. Nuförtiden finns det modernare bibliotek som Apache's HttpClient eller OkHttp som förenklar webbskrapning och API-användning. Dessa bibliotek erbjuder fördelar som inbyggd stöd för HTTPS, cookieshantering och automatisk omdirigering. När du implementerar en webbsida nedladdare, tänk på respons- och anropshanteringen, felhantering och potentiella säkerhetsrisker.

## Se även:
- [Apache HttpComponents](https://hc.apache.org/)
- [OkHttp](https://square.github.io/okhttp/)
- [JSoup](https://jsoup.org/) för HTML-parsing
- [HTMLUnit](http://htmlunit.sourceforge.net/) för att simulera en webbläsare i Java

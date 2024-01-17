---
title:                "Hämta en webbsida"
html_title:           "Java: Hämta en webbsida"
simple_title:         "Hämta en webbsida"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ladda ner en webbsida är ett vanligt förfarande för programmerare. Det innebär att man hämtar ned innehållet från en specifik webbadress och sparar det på sin egen dator. Det kan vara användbart för att arkivera eller bearbeta information från en webbsida.

## Så här gör du:
```Java
public class LaddaNedWebbsida {
    public static void main(String[] args) throws IOException {
        // Ange webbadressen som en sträng
        String adress = "https://www.example.com";
        // Skapa en anslutning till webbadressen
        URL url = new URL(adress);
        // Öppna en ström för att läsa innehållet
        BufferedReader in = new BufferedReader(new InputStreamReader(url.openStream()));
        // Loopa genom varje rad av innehållet och skriv ut det
        String rad;
        while ((rad = in.readLine()) != null) {
           System.out.println(rad);
        }
        // Stäng strömmen när allt är läst
        in.close();
    }
}
```
Output:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## Djupdykning:
Det finns många olika sätt att ladda ner en webbsida på. En vanlig metod är att använda Java-klassen URL för att skapa en anslutning till webbadressen och sedan läsa innehållet rad för rad med BufferedReader. Det finns också andra bibliotek och ramverk, som Apache HttpClient, som kan användas för att ladda ner webbsidor.

Det är också viktigt att tänka på eventuella tillstånd eller begränsningar som kan finnas för nedladdning av webbsidor. Vissa webbplatser kan ha begränsningar för hur ofta man kan hämta innehållet, eller det kan finnas upphovsrättsliga aspekter att ta hänsyn till. Det är alltid bäst att kontrollera med webbplatsens ägare innan man börjar ladda ner deras innehåll.

## Se även:
- Java URL-klassens dokumentation: https://docs.oracle.com/javase/10/docs/api/java/net/URL.html
- Apache HttpClient: https://hc.apache.org/httpcomponents-client-ga/index.html
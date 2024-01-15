---
title:                "Skicka en http förfrågan"
html_title:           "Java: Skicka en http förfrågan"
simple_title:         "Skicka en http förfrågan"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför

Att skicka en HTTP-begäran är en viktig del av att skapa webbapplikationer och andra digitala tjänster. Det låter din programvara kommunicera med servern som håller din applikation igång och göra åtgärder såsom att hämta och lagra data.

## Hur man gör det 

```Java
// skapa en URL-objekt med målet för din begäran
URL url = new URL("https://example.com/api/data");

// skapa en HTTP-begäran med vald metod (t.ex. GET, POST, PUT)
HttpURLConnection connection = (HttpURLConnection) url.openConnection();
connection.setRequestMethod("GET");

// läs in svaret från servern
BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
String inputLine;
StringBuilder response = new StringBuilder();
while ((inputLine = in.readLine()) != null) {
    response.append(inputLine);
}
in.close();

// skriv ut svaret
System.out.println(response.toString());
```

Det här exemplet visar hur man skapar en grundläggande HTTP-begäran med Java. Beroende på dina behov och användningsfall, kan du också behöva lägga till andra parametrar till din begäran, till exempel HTTP-headerns innehåll eller URL-parametrar.

## Djupdykning

HTTP-begäran är uppbyggd av flera delar som är nödvändiga för att skapa en framgångsrik förfrågan och få tillbaka ett svar från servern. Några av dessa delar inkluderar URL, metod (GET, POST, PUT, DELETE), HTTP-header och eventuella data som behövs för att utföra åtgärden. Det är också viktigt att förstå HTTP-statuskoderna som returneras från servern för att kunna hantera eventuella fel eller problem som kan uppstå.

## Se även

- [Java - Skapa en HTTP-begäran](https://www.w3schools.com/java/java_http_request.asp)
- [HTTP Request med Java](https://www.baeldung.com/java-http-request)
- [Vad är HTTP?](https://developer.mozilla.org/sv-SE/docs/Web/HTTP/Overview)
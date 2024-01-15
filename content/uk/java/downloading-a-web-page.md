---
title:                "Завантаження веб-сторінки"
html_title:           "Java: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Для чого
Завантаження веб-сторінки може бути корисним для отримання важливої інформації або автоматизації задач.

## Як
Код для завантаження веб-сторінки виглядає наступним чином:

```java
import java.io.*;  
import java.net.*;

public class DownloadPage {  
    public static void main(String[] args)  throws Exception {    
        String url = "https://example.com"; 
        URL website = new URL(url); 
        URLConnection connection = website.openConnection(); 
        BufferedReader in = new BufferedReader( 
                                new InputStreamReader( 
                                    connection.getInputStream())); 
        String inputLine; 
        while ((inputLine = in.readLine()) != null)  
            System.out.println(inputLine); 
        in.close(); 
    } 
} 
```
Приклад введення:

```
https://example.com
```

Виведе веб-сторінку в консолі.

## Поглиблення
У Java є багато бібліотек для завантаження веб-сторінок, таких як JSoup і Apache HttpClient. Ці бібліотеки дозволяють здійснювати більш складні дії, такі як робота з формами та куки.

## Дивіться також
- [Офіційна документація Java](https://docs.oracle.com/javase/10/docs/api/java/net/URLConnection.html)
- [JSoup](https://jsoup.org/)
- [Apache HttpClient](https://hc.apache.org/httpcomponents-client-ga/)
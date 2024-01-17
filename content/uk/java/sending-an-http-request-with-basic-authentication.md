---
title:                "Надсилання запиту http з базовою аутентифікацією"
html_title:           "Java: Надсилання запиту http з базовою аутентифікацією"
simple_title:         "Надсилання запиту http з базовою аутентифікацією"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що & Чому?
З комунікаційного протоколу HTTP ви можете відправити запит на сервер, щоб отримати певні дані. Іноді, для захисту цих даних, потрібно надіслати запит з обов'язковим аутентифікаційним ключем. Це дозволяє серверу перевірити вашу ідентичність та дозволити доступ до цінних даних. Це важливий інструмент для програмістів, які працюють з веб-додатками та API.

## Таке собі: 
```java
URL url = new URL("http://example.com"); 

HttpURLConnection connection = (HttpURLConnection) url.openConnection(); 

String username = "username"; 
String password = "password"; 
String authentication = username + ":" + password; 
String basicAuth = "Basic " + new String(Base64.getEncoder().encode(authentication.getBytes())); 

connection.setRequestProperty("Authorization", basicAuth); 
String response = ""; 
try(InputStream is = connection.getInputStream()) { 
Scanner sc = new Scanner(is); 
while(sc.hasNextLine()) { 
response += sc.nextLine(); } } 

System.out.println("Output: " + response);
```

## Глибоке врязування:
HTTP-протокол був створений у 1991 році та став основним засобом комунікації для веб-додатків. Це стало узагальненням відправки запитів серверам, щоб отримати відповіді. Одним з альтернативних методів аутентифікації є механізм "Bearer", який використовує токени доступу замість стандартного способу базової аутентифікації. Щоб імплементувати відправку запита з базовою аутентифікацією, необхідно створити об'єкт URL та додати заголовок з аутентифікаційним ключем до запиту.

## Дивіться також:
- [Документація Java по класу HttpURLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Стаття про базову аутентифікацію на сайті Mozilla](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
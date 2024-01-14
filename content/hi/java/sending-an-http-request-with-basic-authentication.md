---
title:                "Java: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Kyun
HTTP request bhejna bahut ahem hai kyunki yeh server se information receive karne ka ek asaan tareeka hai. Isko basic authentication ke saath bhejna security ki guarantee deta hai.

## Kaise Karein
Coding dwaara HTTP request bhejne ka sabse asaan aur reliable tareeka hai. Yeh basic authentication ko include karta hai jiske saath username aur password ki authentication hoti hai. Neeche di gayi Java code blocks examples aur output aapko iss process ko samajhne mein madad karegi.

```Java
// HTTP request object create karein
HttpClient httpClient = HttpClient.newHttpClient();
HttpRequest request = HttpRequest.newBuilder()
    // URL set karein
    .uri(URI.create("https://example.com"))
    // Request method set karein
    .method("GET", HttpRequest.BodyPublishers.noBody())
    // Authentication header set karein
    .header("Authorization", "Basic " + Base64.getEncoder().encodeToString("username:password".getBytes()))
    .build();
// Response object receive karein
HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
// Response code aur body print karein
System.out.println(response.statusCode());
System.out.println(response.body());
```

### Output:

200
Hello World!

## Gehri Jhaank
HTTP request bhejne ke liye basic authentication ka istemaal karke, aap server ko validate karte ho ki aap ek authorized user hai. Headers mein "Authorization" field mein username aur password encode kar ke bhejne se server aapka identity verify kar sakta hai. Ismein security risk kam ho jaata hai kyunki data encrypted rehta hai.

## See Also
- [Java HTTP request tutorial](https://www.geeksforgeeks.org/sending-http-request-using-java/)
- [Basic authentication in Java](https://www.baeldung.com/java-http-request-basic-authentication)
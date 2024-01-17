---
title:                "Wysyłanie żądania http z podstawową autoryzacją"
html_title:           "Java: Wysyłanie żądania http z podstawową autoryzacją"
simple_title:         "Wysyłanie żądania http z podstawową autoryzacją"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Wysyłanie żądania HTTP z podstawowym uwierzytelnianiem jest procesem pozwalającym na bezpieczny dostęp do zasobów internetowych. Programiści wykorzystują to w celu uwierzytelniania użytkowników i autoryzacji dostępu do różnych stron internetowych.

## Jak to zrobić:

```Java
import java.net.HttpURLConnection;
import java.net.URL;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Base64;

public class HttpExample {

  public static void main(String[] args) throws Exception {
      
    String url = "https://example.com";
    URL obj = new URL(url);
    HttpURLConnection con = (HttpURLConnection) obj.openConnection();
    
    con.setRequestMethod("GET");
    
    String user = "username";
    String password = "password";
    String credentials = user + ":" + password;
    String encodedAuth = Base64.getEncoder().encodeToString(credentials.getBytes());
    
    con.setRequestProperty("Authorization", "Basic " + encodedAuth);
    
    int responseCode = con.getResponseCode();
    
    BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
    String inputLine;
    StringBuffer response = new StringBuffer();
    
    while((inputLine = in.readLine()) != null) {
      response.append(inputLine);
    }
    in.close();
    
    System.out.println(response.toString());
  }
}
```

Przykładowy wynik wyświetli HTML strony internetowej dostępnej pod adresem https://example.com, do której zostało wysłane żądanie z użyciem podstawowego uwierzytelniania.

## Deep Dive:

Wysyłanie żądania HTTP z podstawowym uwierzytelnianiem jest jednym z wielu sposobów, w jaki programiści mogą zapewnić bezpieczny dostęp do zasobów internetowych. Inne metody obejmują m.in. uwierzytelnianie OAuth i uwierzytelnianie z użyciem kluczy API. Wysyłanie żądania z podstawowym uwierzytelnianiem jest możliwe dzięki nagłówkom Authorization i Base64 Encoding, które pozwalają na przesłanie danych uwierzytelniających w formacie zrozumiałym dla serwera. Ta metoda jest często wykorzystywana w aplikacjach mobilnych i jest stosunkowo prosta w implementacji.

## Zobacz też:

- [HTTP Basic Authentication](https://www.w3.org/Protocols/HTTP/1.0/spec.html#BasicAA)
- [Using Basic Authentication in Java HTTP Connections](https://www.baeldung.com/java-http-request)
- [Base64 Encoding in Java](https://docs.oracle.com/javase/8/docs/api/java/util/Base64.html)
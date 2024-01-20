---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Arduino: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie żądania HTTP z podstawową autentykacją to proces, w którym nasza aplikacja rozmawia z serwerem sieciowym, przekazując dane za pomocą protokołu HTTP i jednocześnie udowadniając swoją tożsamość serwerowi. Programiści robią to, aby uzyskać dostęp do zasobów serwera, które są chronione przed anonimowym dostępem. 

## Jak to zrobić:

```Java
import java.net.URL;
import java.net.HttpURLConnection;
import java.util.Base64;

public class Main {
    public static void main(String[] args) throws Exception {
        
        String urlStr = "http://example.com";
        String name = "user";
        String password = "password";

        URL url = new URL(urlStr);
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();

        String authStr = name + ":" + password;
        String authEncoded = Base64.getEncoder().encodeToString(authStr.getBytes());

        connection.setRequestMethod("GET");
        connection.setRequestProperty("Authorization", "Basic " + authEncoded);

        int responseCode = connection.getResponseCode();
        System.out.println("Response Code : " + responseCode);
    }
}
```

Kiedy uruchomisz ten kod, wydrukuje on kod odpowiedzi z serwera, który może informować Cię o powodzeniu lub niepowodzeniu.

## Wgłębna analiza

Wysyłanie żądań HTTP z podstawową autentykacją było popularne, gdy internet był młodszy i mniej złożony. Na przestrzeni lat powstało wiele alternatyw dla tej metody, na przykład autentykacja Digest, OAuth czy tokeny JWT. Każda z nich ma swoje mocne i słabe strony.

Podczas korzystania z podstawowej autentykacji ważne jest, aby pamiętać, że Twoje dane logowania są kodowane, a nie szyfrowane. Oznacza to, że jeśli ktoś przechwyci te dane, może je dekodować i użyć. Dlatego zaleca się stosowanie protokołu HTTPS zamiast HTTP, gdy korzystasz z podstawowej autentykacji.

Za przykładem kodu, który przedstawiliśmy, kryje się wiele szczegółów implementacyjnych. Na przykład metoda `setRequestProperty` ustawia nagłówek HTTP `Authorization` na skonkatenowany ciąg `Basic ` i zakodowany ciąg zawierający Twoje dane logowania.

## Zobacz też

1. [Przewodnik Oracle na temat połączeń HTTP](https://docs.oracle.com/javase/tutorial/networking/urls/index.html)
3. [Dokumentacja JDK na temat klasy Base64](https://docs.oracle.com/javase/8/docs/api/java/util/Base64.html)
4. [Dokumentacja JDK na temat klasy HttpURLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
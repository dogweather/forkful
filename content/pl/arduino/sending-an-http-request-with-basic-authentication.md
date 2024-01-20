---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Arduino: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Wysyłanie żądań HTTP z podstawowym uwierzytelnianiem to technika, którą programiści używają do komunikacji Arduino z internetowymi serwerami API. Niezbędne jest to wtedy, gdy serwer wymaga autentykacji użytkownika przed przetwarzaniem żądania.

## Jak to zrobić:

W poniższym przykładzie, używamy biblioteki ESP8266HTTPClient do wysłania żądania GET do serwera API za pomocą podstawowego uwierzytelniania.

```Arduino
#include <ESP8266HTTPClient.h>

void setup() {
    Serial.begin(115200);

    HTTPClient http;
    
    http.begin("http://example.com"); //Specify the URL
    http.setAuthorization("username", "password"); //Specify username/password
    int httpCode = http.GET();

    if(httpCode > 0){
        String payload = http.getString();
        Serial.println(payload);
    } else {
        Serial.println("Error in HTTP request");
    }
    http.end();
}

void loop() {
}
```
Jeżeli wszystko pójdzie zgodnie z planem, output będzie wyglądał tak:

```Arduino
{
   "username": "John",
   "accessLevel": "admin",
   "accountStatus": "active"
}
```

## W Głębokie Wody

- Kontekst historyczny: HTTP Basic Auth jest jednym z najstarszych sposobów uwierzytelniania w HTTP i jest używany do dziś z powodu swojej prostoty.
- Alternatywy: Inne metody, takie jak uwierzytelnianie typu Digest czy token JWT, oferują więcej warstw zabezpieczeń, ale są bardziej skomplikowane w implementacji.
- Szczegóły dotyczące implementacji: Pamiętaj, że hasła są kodowane za pomocą kodowania base64, co nie jest bezpieczne. Dlatego zawsze używaj połączenia HTTPS, gdy korzystasz z uwierzytelniania podstawowego.

## Zobacz także

3. Poradnik Uwierzytelnianie HTTP ze stroną HTTPS: [https://create.arduino.cc/projecthub/Arduino_Scuola/http-authentication-with-httpclient-2bb90a](https://create.arduino.cc/projecthub/Arduino_Scuola/http-authentication-with-httpclient-2bb90a)
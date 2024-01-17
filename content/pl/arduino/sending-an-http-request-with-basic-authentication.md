---
title:                "Wysyłanie żądania http z uwierzytelnieniem podstawowym"
html_title:           "Arduino: Wysyłanie żądania http z uwierzytelnieniem podstawowym"
simple_title:         "Wysyłanie żądania http z uwierzytelnieniem podstawowym"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Co i dlaczego?

Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem to proces, w którym programista pobiera dane z internetu poprzez przesłanie specjalnego żądania do serwera. Programiści często stosują tę metodę z uwagi na jej prostotę i bezpieczeństwo.

Jak to zrobić:

```Arduino
// Uwierzytelnienie podstawowe
#include <WiFiClientSecure.h>
#include <ArduinoHttpClient.h>

char host[] = "example.com";
char username[] = "username";
char password[] = "password";

void setup() {
  Serial.begin(9600);
  WiFiClientSecure client;
  Serial.println("Connecting to WiFi");
  Serial.println("Sending HTTP request");
  if (client.connect(host, 443)) {
    Serial.println("Connected to server");
    client.println("GET / HTTP/1.1");
    client.print("Authorization: Basic ");
    client.println(base64Encode(username, password));
    client.println("Host: example.com");
    client.println("Connection: close");
    client.println();
  }
}

void loop() {
  // waiting for server response
  while (client.available()) {
    char c = client.read();
    Serial.print(c);
  }
  if (!client.connected()) {
    Serial.println();
    Serial.println("Server disconnected");
    client.stop();
    for(;;);
  }
}

String base64Encode(String username, String password) {
  char auth[100] = {0};
  strcpy(auth, username.c_str());
  strcat(auth, ":");
  strcat(auth, password.c_str());

  char encoded[100] = {0};
  base64_encode(encoded, auth, strlen(auth));

  return String(encoded);
}
```

Głębszy zanurzenie:

Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem jest stosowane od początków sieci WWW, aby zapewnić bezpieczne przesyłanie danych użytkowników. Alternatywnymi metodami są uwierzytelnianie oparte na tokenach lub uwierzytelnianie z użyciem certyfikatów SSL.

Zobacz także:

Dokumentacja biblioteki ArduinoHttpClient: https://www.arduino.cc/en/Reference/HttpClient

Informacje na temat HTTP i uwierzytelnienia: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
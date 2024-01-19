---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Clojure: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Enviar uma requisição HTTP com autenticação básica significa fornecer nome de usuário e senha para acessar um recurso específico. Fazemos isso para restringir o acesso a recursos sensíveis a usuários autorizados.

## Como fazer:

```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "your_SSID";
const char* password = "your_PASSWORD";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.println("Connecting...");
  }
}

void loop() {
  String auth = "user_name:password";
  String encodedAuth = base64::encode(auth);
  HTTPClient http;

  http.begin("http://example.com");
  http.addHeader("Authorization", "Basic " + encodedAuth); 

  int httpCode = http.GET();

  if(httpCode > 0) {
    String payload = http.getString();
    Serial.println(payload);
  }
  http.end();
  delay(5000);
}
```

Modifique o SSID e a senha da sua rede Wi-Fi e o par "usuário:senha". O código se conectará à página solicitada e imprimirá a resposta. 

## Mergulho Fundo

HTTP Basic Authentication é tão antigo quanto a internet. É um método simples de autenticação - não é criptografado, então geralmente é usado sobre HTTPS por segurança.

Existem alternativas, como 'Bearer Tokens', OAuth, e outras formas mais seguras de autenticação quando a informação é sensível.

Na biblioteca ESP8266 para Arduino, a autenticação é implementada adicionando um header HTTP. A string "usuário:senha" é codificada para o formato Base64.

## Veja Também

Visite os seguintes links para mais detalhes e informações relacionadas:

- Documentação da Biblioteca HTTPClient: `https://arduino-esp8266.readthedocs.io/en/latest/esp8266httpclient.html`
- Autenticação HTTP básica explicada: `https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Authentication`
- Autenticação base64 em Arduino: `http://www.cplusplus.com/reference/cstdlib/atob/`
- Alternativas de Autenticação: `https://auth0.com/docs/authorization/overview`
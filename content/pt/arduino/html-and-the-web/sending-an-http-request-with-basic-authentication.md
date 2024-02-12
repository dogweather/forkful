---
title:                "Enviando uma requisição HTTP com autenticação básica"
aliases:
- /pt/arduino/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:00.400318-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando uma requisição HTTP com autenticação básica"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Enviar uma requisição HTTP com autenticação básica significa solicitar acesso a um recurso na web que é protegido por um username e password. Programadores fazem isso para interagir com APIs ou serviços protegidos que requiram credenciais para acesso.

## Como Fazer:
```Arduino
#include <ESP8266WiFi.h>
#include <Base64.h>

const char* ssid = "Seu_SSID";
const char* password = "Sua_Senha";
const char* host = "servidor.com";
const int httpPort = 80;
const char* user = "usuario";
const char* pass = "senha123";

WiFiClient client;

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }

  if (client.connect(host, httpPort)) {
    String auth = user + String(":") + pass;
    String authEncoded = base64::encode(auth);

    String request = String("GET /recurso HTTP/1.1\r\n") +
                     "Host: " + host + "\r\n" +
                     "Authorization: Basic " + authEncoded + "\r\n" +
                     "Connection: close\r\n\r\n";

    client.print(request);

    while (client.connected()) {
      String line = client.readStringUntil('\n');
      if (line == "\r") {
        break;
      }
    }

    String response = client.readString();
    Serial.println(response);
  } else {
    Serial.println("Falha na conexão");
  }
}

void loop() {
}
```
Saída Exemplar:
```
HTTP/1.1 200 OK
Content-Type: application/json
Connection: close

{"mensagem": "Acesso concedido."}
```

## Aprofundamento
A autenticação básica HTTP é um método antigo, mas ainda em uso para proteger web services. Baseia-se na codificação das credenciais com Base64, contudo, é considerada insegura se não usada em conjunto com HTTPS, dado que pode ser facilmente decodificada. Alternativas mais seguras incluem OAuth e tokens JWT. Nessa implementação no Arduino, é essencial o uso da biblioteca Base64 para codificar o username e password. Além disso, a ESP8266WiFi library é necessária para estabelecer a conexão Wi-Fi.

## Veja Também
- Documentação ESP8266WiFi: https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html
- Base64 encoding e HTTP authentication: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization
- Segurança em autenticação HTTP: https://owasp.org/www-community/controls/Basic_Authentication

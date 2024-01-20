---
title:                "Enviando uma solicitação http"
html_title:           "Bash: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Enviar um pedido HTTP é o ato de solicitar dados a um servidor web. Programadores fazem isso para recuperar informações úteis, como temperaturas do tempo, dados de status de sistemas, entre outros.

## Como Fazer:

Aqui está um exemplo básico de como você pode enviar um pedido GET HTTP no Arduino:

```Arduino
#include <WiFi.h>

const char* ssid = "nome da sua rede";
const char* password = "sua senha";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Conectando-se WiFi...");
  }
  
  Serial.println("Conectado WiFi!");
  HTTPClient http;

  http.begin("http://exemplodeapi.com");
  int httpCode = http.GET();
  
  if (httpCode > 0) {
    String payload = http.getString();
    Serial.println(payload);
  }
  else {
    Serial.println("Erro na requisição GET");
  }
  
  http.end();
}

void loop() {}
```

## Plongée Profunda

O protocolo HTTP (HyperText Transfer Protocol, em português Protocolo de Transferência de Hipertexto) foi desenvolvido no CERN, na Suíça, em 1989, para permitir a troca de informações na internet. Hoje, governa a maioria das interações entre servidores e clientes na web.

Uma alternativa ao uso de pedidos HTTP
no Arduino seria o MQTT (Message Queue Telemetry Transport), que é um protocolo de mensagens leve ideal para a IoT (Internet das coisas).

Ao enviar um pedido HTTP a partir de um Arduino, estamos na verdade enviando um pedido através da Ethernet ou do WiFi para o roteador, que então encaminha o nosso pedido para o servidor.

## Veja Também

Para mais exemplos e detalhes sobre pedidos HTTP em Arduino, você pode consultar os seguintes recursos:

1. [Documentação oficial do Arduino HTTPClient](https://www.arduino.cc/en/Tutorial/LibraryExamples/HTTPClient)
2. [Biblioteca HTTP do Arduino no GitHub](https://github.com/espressif/arduino-esp32/tree/master/libraries/HTTPClient)
3. [Tutorial de Programação Arduino - W3Schools](https://www.w3schools.com/arduino/)
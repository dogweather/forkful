---
date: 2024-01-20 17:59:05.751068-07:00
description: "Fazer um pedido HTTP significa pedir ou enviar dados para a web. Programadores\
  \ fazem isso para que Arduinos interajam com o mundo online, como obter dados\u2026"
lastmod: '2024-03-13T22:44:46.836245-06:00'
model: gpt-4-1106-preview
summary: "Fazer um pedido HTTP significa pedir ou enviar dados para a web. Programadores\
  \ fazem isso para que Arduinos interajam com o mundo online, como obter dados\u2026"
title: "Enviando uma requisi\xE7\xE3o HTTP"
---

{{< edit_this_page >}}

## O Que & Porquê?
Fazer um pedido HTTP significa pedir ou enviar dados para a web. Programadores fazem isso para que Arduinos interajam com o mundo online, como obter dados de sensores remotos ou controlar algo à distância.

## Como Fazer:
Para enviar um pedido HTTP, vamos usar uma biblioteca Ethernet para um Arduino com shield Ethernet ou uma biblioteca WiFi para um Arduino com capacidades WiFi.

```Arduino
#include <SPI.h>
#include <Ethernet.h>

// substitua por seus dados de rede
byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };
IPAddress server(192, 168, 1, 1); // endereco IP do servidor

EthernetClient client;

void setup() {
  Ethernet.begin(mac);
  Serial.begin(9600);
  
  delay(1000);
  
  if (client.connect(server, 80)) {
    client.println("GET / HTTP/1.1");
    client.println("Host: 192.168.1.1");
    client.println("Connection: close");
    client.println();
  }
}

void loop() {
  if (client.available()) {
    char c = client.read();
    Serial.write(c);
  }
  
  if (!client.connected()) {
    client.stop();
  }
}
```
Espera-se um monte de HTML ou o que o servidor enviar como resposta.

## Aprofundando o Assunto
Enviar pedidos HTTP não é novidade e é fundamental para a web. Antigamente, só computadores faziam isso, mas hoje até um Arduino pequenino consegue. Alternativas incluem MQTT para IoT ou pedidos HTTPS para segurança adicional. Na implementação, cuidado com o tamanho dos dados, tempo de resposta do servidor e possíveis erros de conexão.

## Veja Também:
- Documentação oficial do Arduino Ethernet Library: https://www.arduino.cc/en/Reference/Ethernet
- Documentação oficial do Arduino WiFi Library: https://www.arduino.cc/en/Reference/WiFi
- Guia sobre protocolo HTTP: https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Overview
- Tutorial sobre MQTT: https://www.hivemq.com/mqtt-essentials/

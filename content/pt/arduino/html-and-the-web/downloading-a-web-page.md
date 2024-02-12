---
title:                "Baixando uma página da web"
aliases: - /pt/arduino/downloading-a-web-page.md
date:                  2024-01-20T17:43:31.196047-07:00
model:                 gpt-4-1106-preview
simple_title:         "Baixando uma página da web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?
Baixar uma página da web é o ato de obter o conteúdo disponível em um endereço específico da internet. Programadores fazem isso para coletar dados, interagir com serviços online ou integrar funcionalidades externas em seus projetos de Arduino.

## How to:
Para baixar uma página web, você precisa de um módulo de WiFi ou Ethernet para conectar seu Arduino à internet. Abaixo está um exemplo simplificado usando um módulo WiFi.

```Arduino
#include <WiFi.h>

// Substitua com as credenciais da sua rede
const char* ssid = "seuSSID";
const char* password = "suaSenha";

const char* host = "www.exemplo.com";

WiFiClient client;

void setup() {
  Serial.begin(115200);
  // Conecta ao WiFi
  WiFi.begin(ssid, password);
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  Serial.println("WiFi conectado");
  Serial.println("Endereço IP: ");
  Serial.println(WiFi.localIP());

  // Conexão ao host
  if (!client.connect(host, 80)) {
    Serial.println("Conexão falhou");
    return;
  }
  
  client.println("GET / HTTP/1.1");
  client.println("Host: " + String(host));
  client.println("Connection: close");
  client.println();
  
  while (client.connected()) {
    String line = client.readStringUntil('\n');
    if (line == "\r") {
      Serial.println("Headers received");
      break;
    }
  }
  
  String line;
  while (client.available()) {
    line = client.readStringUntil('\n');
    Serial.println(line);
  }
}

void loop() {
  
}
```

```Sample Output
.
.
WiFi conectado
Endereço IP: 
192.168.1.123
Headers received
<!DOCTYPE html>
<html>
<body>
<!-- Conteúdo da página aqui -->
</body>
</html>
```

## Deep Dive
Quando a web começou a se expandir nos anos 90, era difícil imaginar microcontroladores como o Arduino baixando páginas da web. Hoje, com módulos WiFi/ Ethernet acessíveis, é simples integrar o Arduino à internet. Há alternativas ao modelo mostrado, como usar o protocolo HTTPS para páginas seguras ou bibliotecas específicas para lidar com requisitos complexos, como o ParseWebpage. A implementação envolve enviar um pedido HTTP GET e processar a resposta. O código deve lidar com timeouts e erros de conexão para maior robustez.

## See Also
- [Arduino HttpClient Library](https://www.arduino.cc/en/Tutorial/LibraryExamples/HttpClient)
- [ESP8266 WiFi Module](https://www.esp8266.com/)
- [Arduino Ethernet Shield Tutorial](https://www.arduino.cc/en/Guide/ArduinoEthernetShield)
- [HTTP Made Really Easy](https://www.jmarshall.com/easy/http/) (em inglês)

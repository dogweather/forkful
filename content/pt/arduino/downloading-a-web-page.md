---
title:                "Baixando uma página da web"
html_title:           "Arduino: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

O que e por que fazer download de uma página web
Fazer download de uma página web basicamente significa baixar seu conteúdo e salvá-lo em seu computador. Isso é comumente feito por programadores com o objetivo de armazenar conteúdos para uso posterior em seus projetos.

## Como fazer:
```Arduino
#include <WiFi.h> // inclui a biblioteca WiFi

const char* ssid = "SUA REDE WIFI"; // altere para o nome da sua rede
const char* password = "SENHA DA REDE"; // altere para a senha da sua rede

void setup() {
  Serial.begin(115200); // inicia a comunicação serial
  WiFi.begin(ssid, password); // conecta na rede WiFi
  while (WiFi.status() != WL_CONNECTED) { // espera pela conexão
    delay(1000);
    Serial.println("Conectando na rede WiFi...");
  }
}

void loop() {
  WiFiClient client; // cria um cliente WiFi
  const char* host = "www.pagina-web.com"; // altere para o endereço da página web desejada
  if (!client.connect(host, 80)) { // tenta se conectar ao servidor da página
    Serial.println("Falha ao se conectar ao servidor.");
    return;
  }

  client.print(String("GET /pagina.html HTTP/1.1\r\n") + // faz uma requisição GET pela página
               "Host: " + host + "\r\n" +
               "Connection: close\r\n\r\n"); // cabeçalhos de requisição

  while (client.connected()) { // leitura dos dados recebidos
    if (client.available()) {
      String line = client.readStringUntil('\r');
      Serial.print(line);
    }
  }

  client.stop(); // desconecta do servidor da página

  delay(60000); // espera 1 minuto antes de repetir o processo
}
```

## Mais informações:
Para fazer download de uma página web, é necessário ter uma conexão à internet e saber o endereço da página desejada. Existem também outras formas de fazer download de páginas web, como através de comandos no terminal ou com softwares externos. É importante lembrar que qualquer conteúdo baixado deve ser utilizado conforme as leis de direitos autorais.

## Dica:
Para verificar se a conexão com a rede WiFi foi estabelecida, você pode usar a função ```WiFi.status()``` que retorna o estado da conexão.

## Veja também:
- [Guia para iniciantes em Arduino](https://www.arduino.cc/en/Guide/ArduinoUno)
- [Documentação da biblioteca WiFi](https://arduino.github.io/arduino-esp32/versions/1.0.1-rc2/classWiFi.html)
- [Tutorial de download de páginas web com ESP32](https://randomnerdtutorials.com/esp32-web-server-spiffs-spi-flash-file-system/)
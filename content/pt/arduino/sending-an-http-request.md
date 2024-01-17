---
title:                "Enviando uma solicitação http"
html_title:           "Arduino: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O que & Porquê?
Enviar uma solicitação HTTP significa que estamos pedindo para que algo seja realizado por um servidor web. Isso é feito através do protocolo HTTP (Hypertext Transfer Protocol), que é a base da comunicação da internet. Programadores frequentemente enviam solicitações HTTP para obter ou enviar informações do ou para o servidor, como por exemplo ao acessar uma página da web ou enviar dados de um formulário.

## Como fazer:
```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h> // Biblioteca para enviar solicitações HTTP

const char* ssid = "NOME_DA_REDE";
const char* password = "SENHA_DA_REDE";

void setup () {
  Serial.begin(115200);
  WiFi.begin(ssid, password); // Conectar ao WiFi
  while (WiFi.status() != WL_CONNECTED) { // Aguardar até estar conectado
    delay(500);
  }
  Serial.println("Conectado ao WiFi com sucesso!")
  
  HTTPClient http; // Criar objeto HTTPClient
  http.begin("https://www.example.com"); // Especificar o endereço do servidor
  int httpCode = http.GET(); // Enviar solicitação GET ao servidor
  String response = http.getString(); // Obter resposta do servidor
  Serial.println(response); // Imprimir a resposta no monitor serial
  http.end(); // Encerrar a conexão HTTP
}

void loop() {}

``` 
#### Saída do Monitor Serial:
```
HTTP/1.1 200 OK
Date: Thu, 29 Apr 2021 00:00:00 GMT
Server: example-server
Connection: close
Content-Type: text/html; charset=UTF-8

<html>
<head>
...
</head>
<body>
...
</body>
</html>
```

## Profundidade:
Existem várias alternativas para enviar solicitações HTTP em placas Arduino, como por exemplo a biblioteca cURL e a biblioteca ESP8266HTTPClient utilizada no exemplo acima. A utilização do protocolo HTTPS (Hypertext Transfer Protocol Secure) também é comum para aumentar a segurança durante a comunicação com o servidor. É importante verificar a documentação do servidor para entender como enviar uma solicitação corretamente, já que diferentes servidores podem ter diferentes requisitos.

## Veja também:
- [Documentação da biblioteca WiFi do ESP8266](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html)
- [Documentação da biblioteca HTTPClient do ESP8266](https://github.com/esp8266/Arduino/blob/master/libraries/ESP8266HTTPClient/src/ESP8266HTTPClient.h)
- [Tutorial completo sobre como enviar solicitações HTTP com o ESP8266](https://circuits4you.com/2018/02/04/esp8266-http-get-request-example-arduino/)
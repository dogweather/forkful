---
title:                "Arduino: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que enviar uma solicitação HTTP com o Arduino?

Se você já trabalhou com o Arduino, provavelmente sabe que ele é capaz de se comunicar com outros dispositivos através de diferentes protocolos. Mas por que você precisaria enviar uma solicitação HTTP com o Arduino?

Bem, enviar uma solicitação HTTP é uma forma de fazer uma comunicação com a internet. Com isso, você pode coletar dados de diferentes fontes, como APIs, bancos de dados e páginas da web. Com sua capacidade de fazer a coleta de dados e se comunicar com outros dispositivos, o Arduino pode ser um aliado poderoso em projetos de Internet das Coisas (IoT).

## Como enviar uma solicitação HTTP com o Arduino

O processo de enviar uma solicitação HTTP com o Arduino é relativamente simples. Primeiro, você precisará de um módulo Wi-Fi ou Ethernet para se conectar à internet. Em seguida, você precisará de uma biblioteca para fazer a comunicação HTTP, como a biblioteca "HTTPClient" para o Arduino. Agora, vamos dar uma olhada em um exemplo de como enviar uma solicitação HTTP com o Arduino usando a biblioteca "HTTPClient":

```
#include <WiFi.h>
#include <HTTPClient.h>

const char* ssid = "SEU_SSID";
const char* password = "SUA_SENHA";

void setup() {
  Serial.begin(115200);

  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.println("Conectando à rede Wi-Fi...");
  }
}

void loop() {
  if (WiFi.status() == WL_CONNECTED) {  // se estiver conectado
    HTTPClient http;

    http.begin("URL_DO_SEU_SERVIDOR");  // insira aqui a URL do seu servidor

    int httpCode = http.GET();  // envia uma solicitação GET HTTP e recebe o código de resposta

    if (httpCode == HTTP_CODE_OK) {  // se o código de resposta for "ok"
      String response = http.getString();  // salva a resposta em uma variável

      Serial.println(response);  // imprime a resposta no Monitor Serial
    }

    http.end();  // finaliza a conexão HTTP
  }

  delay(5000);  // espera 5 segundos antes de fazer outra solicitação
}
```

Nesse exemplo, estamos nos conectando à internet, acessando uma URL específica e armazenando a resposta em uma variável. Você pode modificar esse código conforme necessário para se adequar aos seus projetos.

## Aprofundando no envio de solicitações HTTP com o Arduino

Agora que você já tem uma ideia básica de como enviar uma solicitação HTTP com o Arduino, vamos nos aprofundar um pouco mais nesse processo.

Uma solicitação HTTP geralmente consiste em um cabeçalho (header) e um corpo (body). O cabeçalho contém informações sobre a solicitação, como o método HTTP (GET, POST, PUT, DELETE), uma URL e os cabeçalhos de autenticação ou cookies, se necessário. O corpo contém os dados da solicitação, como os parâmetros de uma requisição POST. É importante entender essas partes de uma solicitação HTTP para poder enviar solicitações corretamente com o Arduino.

Além disso, é possível usar um servidor local para receber e processar solicitações HTTP do Arduino. Isso pode ser útil em projetos em que você não precisa se comunicar com a internet, mas ainda deseja usar a estrutura do protocolo HTTP para enviar e receber dados.

## Veja também

- Documentação oficial do Arduino: https://www.arduino.cc/reference/en/libraries/httpclient/
- Tutorial de envio de solicitações HTTP com o Arduino: https://randomnerdtutorials.com/esp32-http-get-post-arduino/
- Exemplo de servidor local para comunicação com o Arduino: https://circuits4you.com/2019/05/26/esp32-http-post-get-body-parameters-using-url/
---
title:                "Analise de HTML"
html_title:           "Arduino: Analise de HTML"
simple_title:         "Analise de HTML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Por que?

Você pode estar se perguntando por que alguém se envolveria em analisar HTML usando um dispositivo Arduino. Bem, existem várias razões possíveis. Uma delas é que o Arduino é uma plataforma acessível e flexível, permitindo que desenvolvedores e entusiastas criem projetos criativos e úteis. Além disso, a análise de HTML pode ser útil em diversas aplicações, como em dispositivos IoT, sensores e até mesmo robôs.

## Como fazer

Para começar a analisar HTML com o Arduino, você precisará de alguns componentes básicos, como um Arduino board, uma placa de prototipagem, cabos jumper e um display LCD (opcional). Além disso, é importante ter algum conhecimento básico de programação em Arduino. Com isso em mãos, vamos para o código!

```Arduino
#include <SPI.h>
#include <WiFi.h>
#include <HTTPClient.h>
#include <ArduinoJson.h>

String url = "https://exemplo.com";

void setup() {
  //Inicializa o display LCD, se estiver usando
  Serial.begin(115200);
  delay(10);

  // Conecta ao Wi-Fi
  Serial.print("Conectando ao Wi-Fi...");
  WiFi.begin("nome_da_rede", "senha_da_rede");

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  Serial.println("Conectado com sucesso!");

  //Realiza a requisição HTTP
  HTTPClient http;
  http.begin(url);
  int httpCode = http.GET();

  if (httpCode > 0) {
    String payload = http.getString();

    // Analisa o JSON com o ArduinoJSON
    DynamicJsonDocument doc(1024);
    deserializeJson(doc, payload);

    String title = doc["title"];
    String author = doc["author"];

    Serial.println("Título: " + title);
    Serial.println("Autor: " + author);
  }
  else {
    Serial.println("Falha na conexão!");
  }

  http.end();
}

void loop() {
  // Coloque aqui qualquer código que precise ser executado repetidamente
}
```

Executando esse código, você poderá ver os dados do HTML na porta serial do Arduino. Isso pode ser útil para extrair informações específicas de uma página, como previsão do tempo, cotação de moedas, entre outros.

## Mergulho profundo

Para entender melhor o processo de análise de HTML com o Arduino, é importante conhecer algumas bibliotecas utilizadas no exemplo acima. A biblioteca WiFi permite a conexão com a internet, enquanto a HTTPClient é responsável por realizar as requisições HTTP. Já a ArduinoJSON é utilizada para analisar dados em formato JSON, muito comum em páginas da web.

Além disso, existem outras maneiras de analisar HTML com o Arduino, como utilizando a biblioteca ESP8266WebServer, por exemplo. É importante explorar e experimentar diferentes métodos para encontrar o mais adequado para o seu projeto.

## Veja também

- [Guia do iniciante do Arduino](https://www.arduino.cc/en/Guide/HomePage)
- [Documentação da biblioteca HTTPClient](https://github.com/espressif/arduino-esp32/tree/master/libraries)
- [Tutorial de análise de JSON com o Arduino](https://randomnerdtutorials.com/decoding-and-encoding-json-with-arduino-or-esp8266/)
- [Projeto prático de análise de HTML com o Arduino](https://create.arduino.cc/projecthub/Ujwal_Bhattacharya/arduino-based-web-page-reader-eaf1ca)
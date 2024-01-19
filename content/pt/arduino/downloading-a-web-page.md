---
title:                "Baixando uma página da web"
html_title:           "Bash: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Baixar uma página da web é o processo de recuperar e armazenar o conteúdo de um site no seu dispositivo. Programadores fazem isso para analisar ou manipular esses dados nas aplicações.

## Como fazer:

Vamos usar a biblioteca ESP8266WiFi para baixar uma página da web com este código.

```Arduino
#include <ESP8266WiFi.h>

const char* ssid     = "seu_SSID";
const char* password = "sua_senha";

const char* host = "www.exemplo-website.com";

void setup() {
  Serial.begin(115200);
  delay(100);

  // Conexão Wi-Fi
  Serial.println();
  Serial.println();
  Serial.print("Conectando a ");
  Serial.println(ssid);
  
  WiFi.begin(ssid, password);
  
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }

  Serial.println("");
  Serial.println("WiFi conectado");
  Serial.println("Endereço IP: ");
  Serial.println(WiFi.localIP());
}

void loop() {
  WiFiClient client;
  const int httpPort = 80;
  if (!client.connect(host, httpPort)) {
    Serial.println("Falha na conexão");
    return;
  }

  // Pedido HTTP
  client.print(String("GET /") + " HTTP/1.1\r\n" +
               "Host: " + host + "\r\n" + 
               "Connection: close\r\n\r\n");
  delay(500);

  // Leitura da resposta
  while(client.available()){
    String line = client.readStringUntil('\r');
    Serial.print(line);
  }  
}
```
Após rodar o código, irá conectar à rede e baixar o conteúdo do site.

## Mergulho Profundo

Assembler e C foram as primeiras linguagens a permitir o download de páginas web, seguidos por Python e outros através de bibliotecas HTTP. Hoje, fazemos isso em nossa programação diária para tarefas de web scraping, para realizar análises de dados e até mesmo para funções simples de CRUD.

Existem alternativas ao método acima, como usar a biblioteca HttpClient do Arduino para lidar com comunicações HTTP. Ou até, python com a biblioteca BeautifulSoup para web scraping.

Os detalhes de implementação incluem a necessidade de uma boa gestão de memória ao trabalhar com o ESP8266 devido à sua limitação de memória. Ele também é importante notar o tratamento de possíveis exceções de conexão e ler adequadamente a resposta do servidor.

## Veja também

- [Biblioteca HttpClient](https://www.arduino.cc/en/Tutorial/LibraryExamples/HttpClient)
- [ESP8266WiFi](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html)
- [Python e BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
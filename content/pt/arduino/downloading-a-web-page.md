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

## Por que

Se você deseja integrar seus projetos em Arduino com a web, uma das maneiras de fazer isso é baixando uma página da web diretamente para seu dispositivo. Com o uso da programação, é possível ter acesso a informações e conteúdos da internet, adicionando uma camada de conectividade ao seu projeto.

## Como fazer

```Arduino
#include <Ethernet.h> //inclui a biblioteca Ethernet para a comunicação com a internet

//define o tamanho máximo da página a ser baixada
#define TAMANHO_MAXIMO 1500

byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED }; //endereço MAC do dispositivo
IPAddress ip(192,168,1,10); //endereço IP do dispositivo
EthernetClient cliente; //define o cliente Ethernet

void setup() {
  Ethernet.begin(mac, ip); //inicializa a conexão Ethernet
  Serial.begin(9600); //inicializa a comunicação serial
  delay(1000); //aguarda um segundo para que o dispositivo se conecte à rede
}

void loop() {
  char servidor[] = "nomedosite.com"; //endereço do servidor do site a ser baixado
  if (cliente.connect(servidor, 80)) { //conecta com o servidor na porta 80
    cliente.println("GET /pagina.html HTTP/1.1"); //faz a requisição da página
    cliente.println("Host: nomedosite.com"); //especifica o host
    cliente.println("Connection: close"); //encerra a conexão após a requisição
    cliente.println(); //linha vazia necessária para finalizar o cabeçalho
  }
  
  //lê e imprime a resposta do servidor
  while (cliente.available()) {
    char c = cliente.read();
    Serial.print(c);
  }
  
  //encerra a conexão após receber a resposta completa
  if (!cliente.connected()) {
    Serial.println();
    Serial.println("Conexão encerrada.");
    cliente.stop();
  }
  
  //aguarda 10 segundos para realizar a próxima requisição
  delay(10000); 
}
```

## Aprofundando

Além do exemplo apresentado, também é possível realizar o download de páginas utilizando HTTP Requests, que permitem fazer requisições específicas como POST e PUT. Outra alternativa é utilizar a biblioteca WiFiNINA, que permite a conexão com a internet utilizando uma placa WiFi integrada ao dispositivo. É importante lembrar que para realizar o download de páginas, é necessário ter uma conexão estável e compatível com protocolos de internet.

## Veja também

- [Arduino - Biblioteca Ethernet](https://www.arduino.cc/en/Reference/Ethernet)
- [Arduino - HTTP Request](https://www.arduino.cc/en/Reference/HTTPClient)
- [Arduino - Biblioteca WiFiNINA](https://www.arduino.cc/en/Reference/WiFiNINA)
---
title:                "Arduino: Fazendo o download de uma página da web"
simple_title:         "Fazendo o download de uma página da web"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

##Por que baixar uma página da web utilizando Arduino?

Baixar uma página da web utilizando Arduino pode ser útil para projetos que necessitam de acesso a informações disponíveis online, como dados meteorológicos, notícias, preços de ações, entre outros. Além disso, pode ser uma forma de adicionar funcionalidades de internet aos seus projetos.

##Como baixar uma página da web utilizando Arduino

Para baixar uma página da web utilizando Arduino, é necessário utilizar a biblioteca "ArduinoHttpClient". Para isso, siga os seguintes passos:

1. Abra a IDE do Arduino e vá em "Sketch > Incluir Biblioteca > Gerenciar Bibliotecas"
2. Na caixa de pesquisa, digite "ArduinoHttpClient"
3. Selecione a biblioteca "ArduinoHttpClient" e clique em "Instalar"
4. Aguarde até que a biblioteca seja instalada
5. Reinicie a IDE do Arduino para que a biblioteca seja carregada corretamente

Após a instalação, você pode utilizar o seguinte código de exemplo para baixar uma página da web e exibir seu conteúdo no monitor serial:

```
#include <Ethernet.h>
#include <EthernetHttpClient.h>

// Definir nome do servidor e caminho do recurso a ser baixado
const char server[] = "www.example.com";
const char resource[] = "/";
 
// Definir uma conexão Ethernet
byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };
IPAddress ip(192, 168, 1, 10); // Insira o IP do seu Arduino
EthernetClient client;
HttpClient http(client, server, 80);
 
void setup() {
    // Inicializar conexão Ethernet
    Ethernet.begin(mac, ip);
}

void loop() {
    // Fazer requisição GET
    http.get(resource);
     
    // Verificar status da resposta
    int statusCode = http.responseStatusCode();
    Serial.println("Status da resposta: " + statusCode);
 
    // Ler conteúdo da resposta e imprimir no monitor serial
    String response = http.responseBody();
    Serial.print("Conteúdo da resposta: ");
    Serial.println(response);
     
    // Aguardar alguns segundos até a próxima requisição
    delay(5000);
}
```

##Aprofundando no assunto

Ao utilizar a biblioteca "ArduinoHttpClient", é possível configurar outras opções de requisição, como o método HTTP (GET, POST, PUT, DELETE), adicionar cabeçalhos e parâmetros à requisição, além de realizar conexões seguras através do protocolo HTTPS.

Além disso, é importante ressaltar que a biblioteca "ArduinoHttpClient" é baseada na biblioteca "HttpClient" do Apache, utilizada amplamente para requisições web. Portanto, é possível encontrar mais informações sobre suas funcionalidades e como utilizá-las em projetos Arduino.

##Veja também

- [Página da documentação oficial da biblioteca "ArduinoHttpClient"](https://github.com/amcewen/HttpClient)
- [Tutorial sobre como utilizar a biblioteca "ArduinoHttpClient"](https://randomnerdtutorials.com/arduino-http-get-request/)
- [Outras opções de bibliotecas HTTP para Arduino](https://www.circuitsathome.com/mcu/50/arduino-ethernet-http-get-response-without-client-library/)
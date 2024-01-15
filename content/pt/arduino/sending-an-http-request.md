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

## Por que
Se você está procurando enviar solicitações HTTP com Arduino, provavelmente está tentando se comunicar com um servidor ou obter informações de uma API. Este recurso pode ser útil para projetos que envolvam a transferência de dados pela internet, como monitoramento remoto ou conectividade com a nuvem.

## Como fazer
Enviar uma solicitação HTTP com Arduino não é tão complicado quanto parece. Tudo o que você precisa é de uma placa Arduino com capacidade de conexão à internet, como o modelo Uno WiFi Rev2, e um módulo WiFi ou Ethernet. Siga os passos abaixo para começar:

1. Certifique-se de ter os cabos de energia e dados adequados para sua placa e módulo WiFi ou Ethernet.

2. Conecte o módulo ao Arduino de acordo com as instruções do fabricante.

3. Abra o IDE do Arduino em seu computador e crie um novo sketch.

4. Importe as bibliotecas necessárias para fazer uma solicitação HTTP. Por exemplo, se você estiver usando o módulo WiFi, inclua as bibliotecas "WiFi.h" e "WiFiClient.h".

5. Defina as constantes que serão usadas em sua solicitação, como a URL do servidor e o caminho da API.

6. Inicialize e conecte-se ao módulo WiFi ou Ethernet na função "setup()". Verifique se a conexão foi estabelecida corretamente.

7. Na função "loop()", crie um objeto de cliente e use o método "connect()" para se conectar ao servidor especificado.

8. Use o método "print()" para enviar sua solicitação HTTP, incluindo o método, caminho e cabeçalhos necessários.

9. Use o método "available()" para verificar se a resposta do servidor está disponível.

10. Leia e processe os dados da resposta usando os métodos "read()" e "findUntil()" conforme necessário.

11. Feche a conexão usando o método "stop()" e retorne para a função "loop()".

Aqui está um exemplo simples de código para enviar uma solicitação GET a uma API usando o módulo WiFi:

```Arduino
#include <WiFi.h>
#include <WiFiClient.h>

// Constantes da solicitação HTTP
const char* SSID = "nome-da-sua-rede";
const char* SSID_PASSWORD = "senha-da-sua-rede";
const char* SERVER_URL = "endereco-do-servidor";
const int SERVER_PORT = 80;
const char* API_PATH = "/api/exemplo";

WiFiClient client;

void setup() {
  // Inicialize o módulo WiFi e conecte-se à rede
  WiFi.begin(SSID, SSID_PASSWORD);
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
  }
  // Verifique se a conexão foi estabelecida
  Serial.println("Conectado à rede WiFi!");
}

void loop() {
  // Crie uma conexão com o servidor
  if (!client.connect(SERVER_URL, SERVER_PORT)) {
    Serial.println("Falha na conexão!");
    return;
  }
  // Envie a solicitação GET com os cabeçalhos necessários
  client.println("GET " + String(API_PATH) + " HTTP/1.1");
  client.println("Host: " + String(SERVER_URL));
  client.println("Connection: close");
  client.println();
  // Leia e processe a resposta do servidor
  while (client.available()) {
    String response = client.readStringUntil('\r');
    Serial.print(response);
  }
  // Feche a conexão
  client.stop();
  // Espere 5 segundos antes de enviar outra solicitação
  delay(5000);
}
```

A saída na Serial Monitor deve ser semelhante a isso:

```
HTTP/1.1 200 OK
Content-Type: application/json

{"dado1": 123, "dado2": "abc"}
```

## Mergulho profundo
Existem várias bibliotecas disponíveis para enviar solicitações HTTP com Arduino, cada uma com suas próprias vantagens e requisitos de hardware. Além disso, códigos de status, como redirecionamentos e erros, devem ser tratados adequadamente para garantir o correto processamento da resposta do servidor. Além disso, considere a segurança ao lidar com dados confidenciais em suas solicitações. É importante ter um bom entendimento de como o protocolo HTTP funciona para enviar solicitações corretamente.

## Veja
---
title:                "Arduino: Enviando uma solicitação HTTP com autenticação básica"
simple_title:         "Enviando uma solicitação HTTP com autenticação básica"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

##Por que

Você pode estar se perguntando por que alguém iria se envolver no processo de enviar uma solicitação HTTP com autenticação básica usando o Arduino. Bem, a resposta é simples: essa funcionalidade pode ser útil para se comunicar com diferentes dispositivos e serviços da Internet, permitindo que você integre seu projeto Arduino com várias plataformas e sistemas.

##Como fazer

Enviar uma solicitação HTTP com autenticação básica usando o Arduino pode parecer intimidante no início, mas é bastante simples. Você precisará seguir os seguintes passos:

```
// Instale a biblioteca do HTTPClient para o seu Arduino
#include <ArduinoHttpClient.h>

// Configurar servidor e porta para onde a solicitação será enviada
const char* server = "www.exemplo.com.br";
int porta = 80;

// Configurar o caminho da URL e as credenciais de autenticação
const char* url = "/api/dados";
const char* user = "seu_usuario";
const char* password = "sua_senha";

// Criar um objeto HTTPClient para fazer a solicitação
WiFiClient wifi;
HttpClient http_client = HttpClient(wifi, server, porta);

// Iniciar conexão WiFi e verificar se está conectado
// Ignoramos o "status" do WiFi aqui para mostrar como fazer a conexão
int wifi_status = WL_CONNECTED;
if (wifi_status == WL_CONNECTED) {
  Serial.println("WiFi conectado!");

  // Enviar solicitação HTTP GET com autenticação
  http_client.beginRequest();
  http_client.get(url, "application/json");
  http_client.sendBasicAuth(user, password);
  int resposta = http_client.responseStatusCode();
  String conteudo = http_client.responseBody();
  http_client.endRequest();

  // Exibir o código de status e conteúdo da resposta
  Serial.print("Código de status HTTP: ");
  Serial.println(resposta);
  Serial.print("Conteúdo da resposta: ");
  Serial.println(conteudo);
} else {
  Serial.println("Erro ao conectar com o WiFi.");
}
```

A saída do monitor serial após a execução do código acima será algo parecido com isto:

```
WiFi conectado!
Código de status HTTP: 200
Conteúdo da resposta: {"id": 12345, "temperatura": 25.5, "umidade": 60}
```

##Aprofundando-se

Para entender melhor como esse processo funciona, é importante ter uma noção básica de como funcionam as solicitações HTTP e a autenticação básica.

Uma solicitação HTTP é um pedido que um cliente (no caso, o Arduino) faz a um servidor (no caso, o endereço do URL especificado) para obter algum tipo de informação. As solicitações HTTP podem ser de diferentes tipos, como GET, POST, PUT, DELETE, entre outros. No exemplo acima, usamos um GET request para obter os dados da temperatura e umidade de um servidor.

A autenticação básica é um meio de segurança utilizado para confirmar a identidade do usuário que está acessando um determinado recurso protegido. Ela funciona enviando um nome de usuário e senha em formato codificado junto com a solicitação HTTP para o servidor. O servidor então verifica se as credenciais estão corretas e, se estiverem, fornece acesso ao recurso.

No código de exemplo, usamos a biblioteca HttpClient para criar e enviar a solicitação com autenticação básica. Primeiro, configuramos o servidor e a porta para onde a solicitação será enviada, e depois especificamos o URL e as credenciais de autenticação. Em seguida, iniciamos a conexão WiFi e enviamos a solicitação GET com as informações de autenticação. Por fim, verificamos o código de status e o conteúdo da resposta.

##Veja também

- [Documentação da biblioteca HttpClient](https://github.com/arduino-libraries/ArduinoHttpClient)
- [Tutorial básico de solicitações HTTP com autenticação no Arduino](https://www.arduino.cc/en/Tutorial/HttpClientBasicAuth)
- [Mais sobre solicitações HTTP](https://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html)
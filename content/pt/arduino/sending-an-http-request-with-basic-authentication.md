---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Arduino: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# O que é e porquê?

Enviar uma solicitação HTTP com autenticação básica é um processo em que o seu código Arduino envia informações para um servidor da web protegido por login e senha. Isso é comumente feito por programadores para acessar dados ou controlar dispositivos remotamente.

# Como fazer:

Para enviar uma solicitação HTTP com autenticação básica no seu código Arduino, você precisará modificar uma função existente, ```client.print () ```, adicionando informações de autenticação no formato "usuário:senha" antes de enviar a solicitação. Veja abaixo um exemplo de código:

```Arduino
#include <Ethernet.h>
#include <SPI.h>

byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED }; //Endereço MAC do seu Arduino
char server[] = "www.example.com"; //Endereço do servidor
int port = 80; //Porta do servidor

EthernetClient client; //Cria um objeto do tipo EthernetClient

void setup() {
  Ethernet.begin(mac); //Inicia a conexão Ethernet
  Serial.begin(9600); //Inicia a comunicação serial
  delay(1000); //Aguarda um segundo
}

void loop() {
  if (client.connect(server, port)) { //Conecta ao servidor
    //Envia a solicitação com autenticação básica
    client.print("GET /path/to/file HTTP/1.1\r\n");
    client.print("Host: www.example.com\r\n");
    client.print("Authorization: Basic YWRtaW46cGFzc3dvcmQ=\r\n"); //Substitua pelo seu usuário e senha codificados em base 64
    client.print("Connection: close\r\n\r\n");
    
    //Aguarda a resposta do servidor
    delay(500);
    //Imprime a resposta no monitor serial
    while (client.available()) {
      char c = client.read();
      Serial.print(c);
    }
  }
  else {
    //Se não conseguir se conectar, exibe uma mensagem de erro
    Serial.println("Erro ao se conectar ao servidor.");
  }
  
  //Espera alguns segundos antes de enviar a próxima solicitação
  delay(5000);
}

```

A saída no monitor serial mostrará a resposta do servidor, que pode ser um código de sucesso (200) ou erro (401). Lembre-se de substituir o usuário e senha no exemplo acima pela sua própria combinação codificada em base 64.

# Mergulho profundo:

Enviar uma solicitação HTTP com autenticação básica é uma técnica comum em programação web e é usada para garantir que apenas usuários autorizados tenham acesso aos dados ou dispositivos remotos. Antes da autenticação básica, a autenticação por cookie era usada, mas isso exigia um servidor que salvasse e gerenciasse os cookies, tornando o processo mais complicado.

Existem alternativas para a autenticação básica, como o uso de tokens de autenticação mais seguros, mas ela ainda é amplamente usada por sua simplicidade e compatibilidade com vários servidores.

Ao enviar uma solicitação HTTP com autenticação básica, é importante lembrar de codificar o usuário e senha em base 64. Isso pode ser feito facilmente online com ferramentas gratuitas de codificação em base 64.

# Veja também:

- [Documentação oficial da classe EthernetClient](https://www.arduino.cc/en/Reference/EthernetClient)
- [Tutorial completo de como enviar uma solicitação HTTP com autenticação básica no Arduino](https://www.pubnub.com/blog/arduino-http-with-authentication-example/)
- [Ferramenta online para codificar em base 64](https://www.base64decode.org/)
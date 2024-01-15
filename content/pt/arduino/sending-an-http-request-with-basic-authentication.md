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

## Por que fazer um pedido HTTP com autenticação básica usando o Arduino

Há muitas razões pelas quais alguém pode querer fazer um pedido HTTP com autenticação básica usando o Arduino. Pode ser para acessar dados de uma API segura, ou até mesmo para controlar dispositivos remotos com autenticação.

## Como fazer um pedido HTTP com autenticação básica usando o Arduino

Fazer um pedido HTTP com autenticação básica usando o Arduino é bastante simples. Siga os passos abaixo para começar:

1. Primeiro, precisamos incluir a biblioteca WiFi e a biblioteca HTTPClient no nosso código:

    ```Arduino
    #include <WiFi.h>
    #include <HTTPClient.h>
    ```

2. Em seguida, defina as suas credenciais de autenticação básica:

    ```Arduino
    String username = "seu_usuario";
    String password = "sua_senha";
    ```

3. Em seguida, precisamos nos conectar à rede WiFi usando o `WiFi.begin()` e inserir as credenciais da sua rede. Certifique-se de colocar este código dentro do loop `setup()`:

    ```Arduino
    void setup() {
        WiFi.begin("nome_da_sua_rede", "sua_senha");
    }
    ```

4. Depois de se conectar com sucesso à sua rede, podemos fazer nosso pedido HTTP usando o objeto `HTTPClient`. Aqui está um exemplo de envio de um pedido GET para uma URL protegida com autenticação básica:

    ```Arduino
    HTTPClient http;
    http.begin("https://exemplo.com/api/dados"); // Insira a URL que deseja acessar
    http.setAuthorization(username, password); // Insira as suas credenciais
    int resposta = http.GET(); // Fazemos o pedido GET e armazenamos a resposta em uma variável
    ```

5. Para verificar se o pedido foi bem-sucedido, podemos imprimir o código de status da resposta e o conteúdo retornado:

    ```Arduino
    Serial.println(resposta); // Imprime o código de status da resposta
    Serial.println(http.getString()); // Imprime o conteúdo retornado
    ```

E pronto! Agora você está fazendo pedidos HTTP com autenticação básica usando o Arduino.

## Mais informações sobre enviar um pedido HTTP com autenticação básica

Um pedido HTTP com autenticação básica envolve adicionar as credenciais de autenticação ao cabeçalho da sua solicitação. Isso é feito usando o método `setAuthorization()` do objeto `HTTPClient` e fornecendo o nome de usuário e senha como parâmetros.

Certifique-se de armazenar suas credenciais de forma segura e evite compartilhá-las publicamente. Caso contrário, suas informações confidenciais podem ser comprometidas.

## Veja também

Aqui estão alguns recursos úteis para obter mais informações sobre como fazer pedidos HTTP com autenticação básica usando o Arduino:

- [Documentação oficial da biblioteca HTTPClient](https://github.com/amcewen/HttpClient)
- [Tutorial sobre como fazer pedidos HTTP usando o Arduino](https://randomnerdtutorials.com/esp32-http-get-post-arduino/)
- [Exemplo de código para enviar um pedido GET com autenticação básica no Arduino](https://www.mischianti.org/2020/01/25/get-web-api-json-arduino/)
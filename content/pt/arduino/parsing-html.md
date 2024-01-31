---
title:                "Análise de HTML"
date:                  2024-01-20T15:30:02.953227-07:00
html_title:           "Arduino: Análise de HTML"
simple_title:         "Análise de HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Interpretar HTML significa extrair informações específicas de uma página web. Programadores fazem isso para automatizar a coleta de dados, monitorar mudanças e integrar recursos da web em outros projetos.

## Como Fazer:

Interpretar HTML com Arduino requer um módulo de Wi-Fi para conectar à internet. Vamos usar a biblioteca ESP8266WiFi e HTTPClient para isso.

```cpp
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "SEU_SSID";
const char* password = "SUA_SENHA";

WiFiClient client;

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while(WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }

  if (WiFi.status() == WL_CONNECTED) {
    HTTPClient http;
    http.begin(client, "http://exemplo.com/pagina.html");
    int httpCode = http.GET();

    if(httpCode > 0) {
      String payload = http.getString();
      Serial.println(payload);
      // Aqui você adicionaria o seu código para interpretar o HTML
    } else {
      Serial.println("Erro na conexão HTTP");
    }

    http.end();
  }
}

void loop() {
  // Normalmente nada é feito no loop para esse tipo de operação
}
```

## Mergulho Profundo

Historicamente, interpretar HTML em dispositivos como o Arduino é desafiador devido à limitada memória e capacidade de processamento. Métodos convencionais de parsing em Python ou JavaScript podem não ser viáveis. Alternativas incluem usar expressões regulares para encontrar padrões específicos ou bibliotecas leves projetadas para esses ambientes com restrições de hardware, como a library TinyXML ou o parser HTML Mini.

Cabe mencionar que interpretar HTML corretamente pode ser complexo, pois o HTML de uma página web pode mudar com o tempo. Automatizar completamente essa tarefa pode requerer manutenção constante das regras de parsing para se adaptarem a essas mudanças.

## Veja Também

- Documentação oficial da ESP8266: https://arduino-esp8266.readthedocs.io/en/latest/
- Tutorial sobre expressões regulares (Regex): https://www.regular-expressions.info/
- Uma discussão sobre parsing de HTML em fóruns do Arduino: http://forum.arduino.cc/index.php?topic=55119.0
- Biblioteca TinyXML para parsing de XML no Arduino: https://github.com/leethomason/tinyxml2

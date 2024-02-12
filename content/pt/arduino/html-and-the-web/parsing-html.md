---
title:                "Analisando HTML"
date:                  2024-02-03T19:11:49.528851-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analisando HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Quê & Por Quê?

Fazer parsing de HTML em projetos Arduino é sobre extrair informações de páginas web. Programadores fazem isso para permitir que seus dispositivos Arduino interajam com a Internet, coletando dados de sites para fins que vão desde automação residencial até monitoramento ambiental.

## Como fazer:

Fazer parsing de HTML no Arduino geralmente exige bibliotecas com um pequeno footprint devido aos recursos limitados do dispositivo. Uma escolha popular para web scraping e parsing é usar as bibliotecas `ESP8266HTTPClient` e `ESP8266WiFi` para o ESP8266, ou seus equivalentes para o ESP32, dado o seu suporte nativo para capacidades Wi-Fi e protocolos HTTP. Aqui está um exemplo básico para buscar e fazer parsing de HTML, assumindo que você está trabalhando com um ESP8266 ou ESP32:

Primeiro, inclua as bibliotecas necessárias:
```cpp
#include <ESP8266WiFi.h> // Para ESP8266
#include <ESP8266HTTPClient.h>
#include <WiFiClient.h>
// Use as bibliotecas análogas do ESP32 se estiver usando um ESP32

const char* ssid = "seuSSID";
const char* password = "suaSENHA";
```

Conecte-se à sua rede Wi-Fi:
```cpp
void setup() {
    Serial.begin(115200);
    WiFi.begin(ssid, password);

    while (WiFi.status() != WL_CONNECTED) {
        delay(1000);
        Serial.println("Conectando...");
    }
}
```

Faça uma solicitação HTTP e faça o parsing de um pedaço simples de HTML:
```cpp
void loop() {
    if (WiFi.status() == WL_CONNECTED) { //Verifique o status da conexão Wi-Fi
        HTTPClient http;  //Declare um objeto da classe HTTPClient

        http.begin("http://exemplo.com");  //Especifique o destino da solicitação
        int httpCode = http.GET();  //Envie a solicitação

        if (httpCode > 0) { //Verifique o código de retorno
            String payload = http.getString();   //Obtenha o payload da resposta da solicitação
            Serial.println(payload);             //Imprima o payload da resposta

            // Faça o parsing de uma parte específica, por exemplo, extraindo o título do payload
            int titleStart = payload.indexOf("<title>") + 7; // +7 para passar da tag "<title>"
            int titleEnd = payload.indexOf("</title>", titleStart);
            String pageTitle = payload.substring(titleStart, titleEnd);

            Serial.print("Título da Página: ");
            Serial.println(pageTitle);
        }

        http.end();   //Feche a conexão
    }

    delay(10000); //Faça uma solicitação a cada 10 segundos
}
```

Saída de exemplo (assumindo que http://exemplo.com tenha uma estrutura HTML simples):
```
Conectando...
...
Título da Página: Domínio de Exemplo
```

Este exemplo demonstra como buscar uma página HTML e extrair o conteúdo da tag `<title>`. Para parsing de HTML mais complexo, considere usar expressões regulares (com cautela devido às restrições de memória) ou funções de manipulação de string para navegar pela estrutura do HTML. Parsing avançado pode exigir abordagens mais sofisticadas, incluindo algoritmos de parsing personalizados adaptados à estrutura específica do HTML com o qual você está lidando, já que o ambiente padrão do Arduino não inclui uma biblioteca de parsing de HTML embutida.

---
title:                "Trabalhando com JSON"
date:                  2024-01-19
html_title:           "Arduino: Trabalhando com JSON"
simple_title:         "Trabalhando com JSON"

category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Trabalhar com JSON significa manipular um formato leve de troca de dados, fácil de ler e escrever para humanos e simples de analisar e gerar para máquinas. Programadores usam JSON para estruturar informações em projetos com Arduino, principalmente em aplicações IoT, onde dados são frequentemente trocados entre sensores e serviços web.

## Como Fazer:

Para trabalhar com JSON no Arduino, você vai precisar usar uma biblioteca como a `ArduinoJson`, que pode ser instalada pelo Library Manager no Arduino IDE. Aqui está um exemplo básico de como criar e analisar JSON:

```Arduino
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);
  
  // Criando um documento JSON
  StaticJsonDocument<200> doc;
  doc["sensor"] = "GPS";
  doc["time"] = 1351824120;
  JsonArray data = doc.createNestedArray("data");
  data.add(48.756080);
  data.add(2.302038);
  
  // Serializando JSON para string
  serializeJson(doc, Serial);

  // Analisando JSON recebido
  const char* json = "{\"sensor\":\"GPS\",\"time\":1351824120,\"data\":[48.756080,2.302038]}";
  DeserializationError error = deserializeJson(doc, json);
  if (error) {
    Serial.print(F("deserializeJson() falhou: "));
    Serial.println(error.f_str());
    return;
  }

  Serial.println(doc["sensor"].as<String>());
}

void loop() {
  // Deixe vazio - apenas para exemplo.
}
```

Saída de exemplo:
```plaintext
{"sensor":"GPS","time":1351824120,"data":[48.756080,2.302038]}
GPS
```

## Mergulho Profundo

JSON, acrônimo de JavaScript Object Notation, foi introduzido em 2001. Apesar de vir do JavaScript, é independente de linguagem, levando a adoção generalizada em sistemas de computação. Enquanto `ArduinoJson` é uma biblioteca muito popular para manipulação de JSON no Arduino, existem alternativas como o `json-streaming-parser`, que pode ser mais adequado para dispositivos com memória extremamente limitada. Ao implementar JSON, é crucial respeitar a estrutura e tipos de dados corretos para evitar erros de análise.

## Veja Também

- Documentação oficial do ArduinoJson: [arduinojson.org](https://arduinojson.org/)
- Tutorial JSON para principiantes: [w3schools.com](https://www.w3schools.com/js/js_json_intro.asp)
- JSON no contexto de APIs RESTful: [restfulapi.net](https://restfulapi.net/json/)
- Especificação JSON: [json.org](https://www.json.org/json-pt.html)

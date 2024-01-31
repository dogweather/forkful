---
title:                "Trabalhando com YAML"
date:                  2024-01-19
html_title:           "Arduino: Trabalhando com YAML"
simple_title:         "Trabalhando com YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## O Que é & Por Quê?

YAML é um formato humanamente legível para dados, criado para configuração de programas. Programadores usam porque é intuitivo e ajuda a organizar informações complexas de forma simples.

## Como Fazer:

Aqui vai um exemplo simples de como ler um arquivo YAML com uma biblioteca YAML para Arduino:

```Arduino
#include <ArduinoYAML.h>

void setup() {
  Serial.begin(9600);
  const char* yaml =
    "servidor:\n"
    "  porta: 80\n"
    "  host: 'exemplo.com'";

  YAML::Node root = YAML::Load(yaml);
  Serial.print("Porta: ");
  Serial.println(root["servidor"]["porta"].as<int>());
  Serial.print("Host: ");
  Serial.println(root["servidor"]["host"].as<String>());
}

void loop() {
  // Nada aqui
}
```

Saída esperada:
```
Porta: 80
Host: exemplo.com
```

## Mergulho Profundo

YAML surgiu em 2001 e significa "YAML Ain't Markup Language". Ele foi pensado como uma alternativa ao XML para ser mais legível e com melhor usabilidade para humanos. Em Arduino, a implementação de YAML é menos comum e mais simplificada que em computadores tradicionais, com foco em configuração e não no armazenamento de grandes volumes de dados. Alternativas incluem JSON e XML, mas YAML se destaca pela sua legibilidade.

## Veja Também

- Documentação oficial do YAML: https://yaml.org/
- Biblioteca ArduinoJson, uma alternativa ao YAML para Arduino: https://arduinojson.org/
- Tutorial sobre como usar JSON com Arduino: https://www.arduino.cc/en/Tutorial/LibraryExamples/Json
- Repositório das bibliotecas YAML para Arduino: https://github.com/esp8266/Arduino/tree/master/libraries/ArduinoYAML

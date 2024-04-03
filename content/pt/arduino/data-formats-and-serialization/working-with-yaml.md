---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:39.861331-07:00
description: "Como Fazer: Trabalhar diretamente com YAML no Arduino n\xE3o \xE9 t\xE3\
  o direto quanto em ambientes de programa\xE7\xE3o de n\xEDvel mais alto, devido\
  \ a restri\xE7\xF5es de\u2026"
lastmod: '2024-03-13T22:44:46.859065-06:00'
model: gpt-4-0125-preview
summary: "Trabalhar diretamente com YAML no Arduino n\xE3o \xE9 t\xE3o direto quanto\
  \ em ambientes de programa\xE7\xE3o de n\xEDvel mais alto, devido a restri\xE7\xF5\
  es de mem\xF3ria e a aus\xEAncia de bibliotecas nativas para processamento de YAML."
title: Trabalhando com YAML
weight: 41
---

## Como Fazer:
Trabalhar diretamente com YAML no Arduino não é tão direto quanto em ambientes de programação de nível mais alto, devido a restrições de memória e a ausência de bibliotecas nativas para processamento de YAML. No entanto, para projetos que requerem análise ou geração de YAML, uma abordagem típica envolve o uso de um computador auxiliar (como um Raspberry Pi) ou a conversão de arquivos YAML para um formato mais amigável ao Arduino (como JSON) usando scripts externos. Para fins de demonstração, vamos focar nesta última abordagem usando uma biblioteca popular: ArduinoJson.

**Passo 1:** Converta sua configuração YAML para JSON. Você pode usar ferramentas online ou utilitários de linha de comando como `yq`.

Arquivo YAML (`config.yaml`):
```yaml
wifi:
  ssid: "SeuSSID"
  password: "SuaSenha"
```

Convertido para JSON (`config.json`):
```json
{
  "wifi": {
    "ssid": "SeuSSID",
    "password": "SuaSenha"
  }
}
```

**Passo 2:** Use a biblioteca ArduinoJson para analisar o arquivo JSON no seu sketch Arduino. Primeiro, você precisa instalar a biblioteca ArduinoJson através do Gerenciador de Bibliotecas no IDE Arduino.

**Passo 3:** Carregue e analise o JSON no seu código. Devido às limitações de armazenamento do Arduino, imagine que a string JSON é armazenada em uma variável ou lida de um cartão SD.

Exemplo de sketch Arduino:
```cpp
#include <ArduinoJson.h>

const char* jsonConfig = "{\"wifi\":{\"ssid\":\"SeuSSID\",\"password\":\"SuaSenha\"}}";

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc;
  DeserializationError error = deserializeJson(doc, jsonConfig);

  if (error) {
    Serial.print(F("deserializeJson() falhou: "));
    Serial.println(error.f_str());
    return;
  }

  const char* ssid = doc["wifi"]["ssid"]; // "SeuSSID"
  const char* password = doc["wifi"]["password"]; // "SuaSenha"

  Serial.print("SSID: ");
  Serial.println(ssid);
  Serial.print("Senha: ");
  Serial.println(password);
}

void loop() {
  // Nada aqui para este exemplo
}
```

Saída ao executar o sketch:
```
SSID: SeuSSID
Senha: SuaSenha
```

Esta abordagem, envolvendo a conversão para JSON e o uso da biblioteca ArduinoJson, permite um manejo de configuração YAML gerenciável dentro de projetos Arduino, contornando a análise direta de YAML no microcontrolador.

---
aliases:
- /pt/arduino/working-with-yaml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:39.861331-07:00
description: "YAML (YAML N\xE3o \xE9 Uma Linguagem de Marca\xE7\xE3o) \xE9 um padr\xE3\
  o de serializa\xE7\xE3o de dados leg\xEDvel por humanos, que pode ser usado para\
  \ arquivos de configura\xE7\xE3o,\u2026"
lastmod: 2024-02-18 23:08:58.429295
model: gpt-4-0125-preview
summary: "YAML (YAML N\xE3o \xE9 Uma Linguagem de Marca\xE7\xE3o) \xE9 um padr\xE3\
  o de serializa\xE7\xE3o de dados leg\xEDvel por humanos, que pode ser usado para\
  \ arquivos de configura\xE7\xE3o,\u2026"
title: Trabalhando com YAML
---

{{< edit_this_page >}}

## O Que & Por Quê?

YAML (YAML Não é Uma Linguagem de Marcação) é um padrão de serialização de dados legível por humanos, que pode ser usado para arquivos de configuração, comunicação entre programas e armazenamento de dados. Programadores recorrem ao YAML em projetos Arduino para simplificar o processo de configuração de suas aplicações, tornando mais fácil a modificação de parâmetros sem aprofundar-se no código, melhorando a legibilidade e simplificando o compartilhamento de configurações.

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

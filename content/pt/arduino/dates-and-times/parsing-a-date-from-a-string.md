---
aliases:
- /pt/arduino/parsing-a-date-from-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:28.555788-07:00
description: "Analisar uma data a partir de uma string no Arduino envolve extrair\
  \ e converter os componentes da data (ano, m\xEAs, dia) de uma representa\xE7\xE3\
  o textual para\u2026"
lastmod: 2024-02-18 23:08:58.418450
model: gpt-4-0125-preview
summary: "Analisar uma data a partir de uma string no Arduino envolve extrair e converter\
  \ os componentes da data (ano, m\xEAs, dia) de uma representa\xE7\xE3o textual para\u2026"
title: Analisando uma data a partir de uma string
---

{{< edit_this_page >}}

## O Que e Por Quê?

Analisar uma data a partir de uma string no Arduino envolve extrair e converter os componentes da data (ano, mês, dia) de uma representação textual para um formato que possa ser utilizado para cronometragem, comparações ou manipulações dentro de sketches. Programadores frequentemente realizam essa tarefa para interfacear com componentes como relógios em tempo real, registradores ou para processar entradas de APIs da web e interfaces de usuário onde as datas possam ser apresentadas em um formato legível.

## Como fazer:

Abordagem direta sem uma biblioteca de terceiros:

```cpp
#include <Wire.h>
#include <RTClib.h>

void setup() {
  Serial.begin(9600);
  // Exemplo de string de data no formato AAAA-MM-DD
  String dateString = "2023-04-01"; 

  int year = dateString.substring(0, 4).toInt();
  int month = dateString.substring(5, 7).toInt();
  int day = dateString.substring(8, 10).toInt();

  // Inicialize um objeto DateTime com os componentes analisados
  DateTime parsedDate(year, month, day);
  
  Serial.print("Data Analisada: ");
  Serial.print(parsedDate.year(), DEC);
  Serial.print("/");
  Serial.print(parsedDate.month(), DEC);
  Serial.print("/");
  Serial.println(parsedDate.day(), DEC);
}

void loop() {}
```

Saída de Amostra:
```
Data Analisada: 2023/4/1
```

Usando uma biblioteca de terceiros (*ArduinoJson* para cenários de análise mais complexos, como obter uma data de uma resposta JSON):

Primeiro, instale a biblioteca ArduinoJson através do Gerenciador de Bibliotecas do Arduino.

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  // Simulando uma resposta JSON
  String jsonResponse = "{\"date\":\"2023-07-19\"}";
  StaticJsonDocument<200> doc;
  deserializeJson(doc, jsonResponse);

  // Extraindo a string da data
  const char* date = doc["date"];

  // Analise a data a partir da string como antes
  int year = String(date).substring(0, 4).toInt();
  int month = String(date).substring(5, 7).toInt();
  int day = String(date).substring(8, 10).toInt();
  
  Serial.print("Data Analisada do JSON: ");
  Serial.print(year);
  Serial.print("/");
  Serial.print(month);
  Serial.print("/");
  Serial.println(day);
}

void loop() {}
```

Saída de Amostra:
```
Data Analisada do JSON: 2023/7/19
```

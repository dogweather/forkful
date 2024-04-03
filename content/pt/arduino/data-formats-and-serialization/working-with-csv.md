---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:52.425541-07:00
description: "Trabalhar com arquivos CSV (Comma-Separated Values - Valores Separados\
  \ por V\xEDrgula) no Arduino envolve ler e escrever em arquivos CSV geralmente\u2026"
lastmod: '2024-03-13T22:44:46.861217-06:00'
model: gpt-4-0125-preview
summary: "Trabalhar com arquivos CSV (Comma-Separated Values - Valores Separados por\
  \ V\xEDrgula) no Arduino envolve ler e escrever em arquivos CSV geralmente armazenados\
  \ em um cart\xE3o SD, possibilitando o registro de dados, configura\xE7\xF5es e\
  \ muito mais."
title: Trabalhando com CSV
weight: 37
---

## Como fazer:
O Arduino não possui uma biblioteca integrada especificamente para manipulação de arquivos CSV, mas você pode usar as bibliotecas `SD` e `SPI` para acessar arquivos em um cartão SD e então analisar ou gerar dados CSV usando técnicas básicas de manipulação de strings. Ao lidar com manipulações mais complexas de CSV, a biblioteca de terceiros `ArduinoCSV` pode ser utilizada para facilitar a análise e escrita.

**Lendo Dados CSV de um Cartão SD:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Inicialização falhou!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      Serial.println(dataLine); // Imprime a linha CSV
    }
    dataFile.close();
  } else {
    Serial.println("Erro ao abrir data.csv");
  }
}

void loop() {
  // Não utilizado neste exemplo
}
```
*Saída de Exemplo:*
```
SensorID, Timestamp, Value
1, 1597840923, 23.5
2, 1597840987, 22.4
```

**Escrevendo Dados CSV em um Cartão SD:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Inicialização falhou!");
    return;
  }
  File dataFile = SD.open("output.csv", FILE_WRITE);
  if (dataFile) {
    dataFile.println("SensorID, Timestamp, Value"); // Cabeçalho CSV
    dataFile.println("1, 1597840923, 23.5"); // Linha de exemplo de dados
    dataFile.close();
    Serial.println("Dados escritos");
  } else {
    Serial.println("Erro ao abrir output.csv");
  }
}

void loop() {
  // Não utilizado neste exemplo
}
```
*Saída de Exemplo:*
```
Dados escritos
```

**Usando ArduinoCSV para Análise:**
Se lidar com arquivos CSV complexos, a biblioteca `ArduinoCSV` pode simplificar significativamente os esforços de análise. Este exemplo presume que você já instalou a biblioteca `ArduinoCSV`.

```cpp
#include <SPI.h>
#include <SD.h>
#include <ArduinoCSV.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Inicialização falhou!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    CSVParser parser;
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      if (parser.parseLine(dataLine)) {
        for (int i = 0; i < parser.count(); i++) {
          Serial.print(parser.getField(i)); // Imprime cada campo
          if (i < parser.count() - 1) {
            Serial.print(", ");
          }
        }
        Serial.println();
      }
    }
    dataFile.close();
  } else {
    Serial.println("Erro ao abrir data.csv");
  }
}

void loop() {
  // Não utilizado neste exemplo
}
```
*Saída de Exemplo:*
```
SensorID,  Timestamp,  Value
1,  1597840923,  23.5
2,  1597840987,  22.4
```
Nesses exemplos, ao ler e escrever em arquivos CSV em um cartão SD, projetos Arduino podem facilmente coletar dados, armazenar configurações ou trocar dados com outras aplicações em um formato universalmente acessível.

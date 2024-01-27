---
title:                "Escrevendo um arquivo de texto"
date:                  2024-01-19
html_title:           "Arduino: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Gravar um arquivo de texto significa salvar dados em um formato legível por humanos no armazenamento de um dispositivo. Programadores fazem isso para registar informações, como dados de sensores, configurações de um programa, ou logs de eventos.

## Como Fazer:
```Arduino
#include <SD.h>

File meuArquivo;

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) { // Supondo que o pino CS do SD esteja no pino 4
    Serial.println("Falha na inicialização do cartão SD");
    return;
  }
  meuArquivo = SD.open("dados.txt", FILE_WRITE);
  
  if (meuArquivo) {
    meuArquivo.println("Salvando este texto no arquivo!");
    meuArquivo.close(); // Sempre feche o arquivo após escrever para salvar as mudanças
  } else {
    Serial.println("Erro ao abrir o arquivo");
  }
}

void loop() {
  // Nada aqui por enquanto
}
```

## Mergulho Profundo
Escrever em arquivos de texto é uma prática que remonta aos primeiros dias da computação, quando era essencial ter uma forma de armazenar e recuperar informações. Embora hoje existam alternativas como bancos de dados ou o armazenamento na nuvem, a simplicidade de gravar em arquivos de texto permanece útil, especialmente em contextos de hardware limitado como do Arduino. O método mostrado usa a biblioteca SD para gravar em um cartão SD, que é frequentemente usado devido ao espaço de armazenamento limitado na maioria dos microcontroladores Arduino.

## Ver Também
- Documentação da biblioteca SD: https://www.arduino.cc/en/Reference/SD
- Tutorial Arduino sobre leitura e escrita de arquivos: https://www.arduino.cc/en/Tutorial/LibraryExamples/ReadWrite
- Página do produto Arduino com dicas e recursos: https://store.arduino.cc/

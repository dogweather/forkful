---
title:                "Escrevendo um arquivo de texto"
html_title:           "Arduino: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Escrever um arquivo de texto é uma maneira comum para os programadores armazenarem e compartilharem informações importantes em seus projetos Arduino. Isso permite que os dados sejam lidos e salvos facilmente, tornando o processo de programação mais eficiente e organizado.

## Como fazer:
Existem várias maneiras de escrever um arquivo de texto usando o Arduino, mas neste artigo, vamos nos concentrar em uma abordagem simples usando a biblioteca SD. Primeiro, você precisará conectar um cartão SD ao seu Arduino, em seguida, siga os passos abaixo:
```Arduino
#include <SD.h>
const int chipSelect = 4;

void setup() {
  Serial.begin(9600);

  // inicializa o cartão SD com o pino chipSelect
  if (!SD.begin(chipSelect)) {
    Serial.println("Falha ao inicializar o cartão SD.");
    return;
  }

  Serial.println("Digite o texto que deseja salvar:");

  // aguarda até receber o texto a ser salvo via Serial
  while(!Serial.available());

  // abre um arquivo para escrita, use o nome que desejar
  File arquivo = SD.open("meu_texto.txt", FILE_WRITE);

  // escreve o texto recebido na Serial no arquivo
  arquivo.println(Serial.readString());

  // fecha o arquivo
  arquivo.close();

  Serial.println("Texto salvo com sucesso!");
}

void loop() {
  // código restante do seu projeto
}
```

Ao carregar esse código no seu Arduino, você poderá enviar o texto que deseja salvar através da porta Serial. Esse texto será salvo no arquivo "meu_texto.txt" no cartão SD conectado. Você também pode alterar o nome do arquivo para o que desejar.

## Mergulho Profundo:
A prática de escrever arquivos de texto em dispositivos de armazenamento externos, como cartões SD, não é uma inovação recente. Na verdade, ela foi usada nos primeiros computadores para salvar informações importantes.

Existem também outras maneiras de escrever arquivos de texto no Arduino, como usando a biblioteca SPIFFS ou o módulo Wifi. No entanto, a abordagem usando a biblioteca SD é uma maneira simples e eficiente de fazer isso.

## Veja Também:
- [Documentação da biblioteca SD do Arduino](https://www.arduino.cc/en/reference/SD)
- [Tutorial de escrita de arquivos de texto no Arduino usando a biblioteca SPIFFS](https://randomnerdtutorials.com/esp32-esp8266-arduino-write-read-file-spiffs/)
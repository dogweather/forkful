---
title:                "Criando um arquivo temporário"
html_title:           "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

A criação de um arquivo temporário é um método que permite armazenar dados de forma temporária durante a execução do programa. Os programadores fazem isso para gerir eficientemente a memória e facilitar certas operações, como a classificação de grandes conjuntos de dados.

## Como Fazer:

Para os nossos propósitos, iremos usar a biblioteca SD embutida com a IDE do Arduino para lidar com a criação de arquivos em um cartão SD.

```Arduino
#include <SD.h>

File tempFile;

void setup() {
  if (!SD.begin(4)) {
    Serial.println("Falha na inicialização do cartão SD!);
    return;
  }

  tempFile = SD.open("temp.txt", FILE_WRITE);
  
  if (tempFile) {
    tempFile.println("Este é um arquivo temporário!");
    tempFile.close();
    Serial.println("Arquivo temporário criado com sucesso!");
  }
  else {
    Serial.println("Falha ao criar o arquivo temporário!");
  } 
}

void loop() {
 // Coloque aqui o seu código.
}
```

Quando executado, o código acima deve resultar no seguinte output:

```Arduino
Arquivo temporário criado com sucesso!
```

## Em Detalhes:

Criação de arquivos temporários é um conceito fundamental na programação, que existe desde os dias antiquados de cartões perfurados até o presente. 

Existem várias maneiras alternativas de criar um arquivo temporário. Algumas bibliotecas de software disponíveis permitem que um programador faça isso, como as bibliotecas de I/O dos sistemas operacionais Linux e Windows.

Quando se trata de detalhes de implementação, a coisa mais importante a ser observada na criação de arquivos temporários em um Arduino é a memória. Arduino tem uma quantidade limitada de memória, por isso, assegure-se de remover ou reutilizar o arquivo temporário uma vez que seus dados não sejam mais necessários.

## Veja Também:

Para mais informações sobre programação de Arduino e manuseio de arquivos, consulte os seguintes links:

1. [Biblioteca Arduino SD](https://www.arduino.cc/en/Reference/SD)
2. [Tutorial de Manipulação de Arquivos com a Biblioteca SD](https://learn.adafruit.com/adafruit-micro-sd-breakout-board-card-tutorial)
3. [Dicas para gerenciar memória no Arduino](https://learn.adafruit.com/memories-of-an-arduino/optimizing-sram)
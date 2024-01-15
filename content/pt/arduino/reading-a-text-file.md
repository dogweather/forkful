---
title:                "Lendo um arquivo de texto"
html_title:           "Arduino: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que

Você pode precisar ler um arquivo de texto em seu projeto Arduino por várias razões. Pode ser necessário armazenar dados importantes, como configurações ou informações de calibração, em um arquivo de texto que possa ser facilmente modificado. Também pode ser necessário ler um arquivo de texto que foi criado por outro dispositivo, como um sensor externo. Independentemente do motivo, saber como ler um arquivo de texto no seu Arduino pode ser útil em vários projetos.

## Como fazer

Ler um arquivo de texto em seu projeto Arduino é um processo relativamente simples. Primeiro, você precisará de um cartão microSD para armazenar o arquivo de texto. Em seguida, siga os passos abaixo para ler o arquivo:

```
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  // Inicializa o cartão SD
  SD.begin(4);

  // Abra o arquivo de texto para leitura
  myFile = SD.open("arquivo.txt");

  // Verifique se o arquivo foi aberto com sucesso
  if (myFile) {
    // Leia e exiba o conteúdo do arquivo
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    // Feche o arquivo
    myFile.close();
  } else {
    // Se o arquivo não foi aberto, exiba uma mensagem de erro
    Serial.println("Erro ao abrir o arquivo.");
  }
}

void loop() {
  // Nada acontece no loop
}
```

Este código irá inicializar o cartão SD, abrir o arquivo de texto e ler o conteúdo até que todo o arquivo seja lido. Em seguida, ele é fechado e, se tudo der certo, o conteúdo será exibido no monitor serial.

## Mergulho profundo

Existem algumas coisas importantes a serem consideradas ao trabalhar com arquivos de texto em um projeto Arduino. Primeiro, certifique-se de que o arquivo de texto está formatado corretamente e que o Arduino possa ler seu conteúdo. Você também deve considerar o tamanho do arquivo e o tamanho do buffer disponível no Arduino. Se o seu arquivo for muito grande, pode ser necessário dividi-lo em partes menores para facilitar a leitura e o processamento pelo Arduino.

Outro ponto importante é garantir a integridade do seu cartão SD. Se o arquivo de texto estiver corrompido ou danificado, o Arduino pode não conseguir lê-lo corretamente. Certifique-se de verificar a saúde do seu cartão SD periodicamente e de fazer backup dos arquivos importantes antes de usá-los em um projeto.

## Veja também

- [Documentação do Arduíno sobre a biblioteca SD](https://www.arduino.cc/en/Reference/SD)
- [Tutorial do Hackster sobre leitura e escrita em arquivos de texto no Arduino](https://www.hackster.io/Salmanfarisvp/arduino-uno-sd-card-reading-and-writing-codes-6d7874)
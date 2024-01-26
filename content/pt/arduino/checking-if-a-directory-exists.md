---
title:                "Verificando se um diretório existe"
html_title:           "Arduino: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Verificar se um diretório existe é simplesmente o ato de checar se um determinado caminho leva a uma pasta conhecida no seu cartão SD ou memória flash. Programadores fazem isso para evitar erros ao tentar acessar ou escrever arquivos em diretórios que não existem.

## Como fazer:
```Arduino
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin()) {
    Serial.println("Falha na inicialização do cartão SD");
    return;
  }
  
  File root = SD.open("/");
  if (root.isDirectory()) {
    File dir = root.openNextFile();
    while (dir) {
      if (dir.isDirectory()) {
        Serial.print("Diretório encontrado: ");
        Serial.println(dir.name());
      }
      dir = root.openNextFile();
    }
  } else {
    Serial.println("Raiz não é um diretório!");
  }
}

void loop() {
  // Não é necessário implementar esse loop
}
```

Sample output:
```
Diretório encontrado: DCIM
Diretório encontrado: MUSICA
```

## Mergulho Profundo
Historicamente, o conceito de verificar a existência de um diretório é importante em muitas plataformas e linguagens de programação, não só em Arduino. Em Arduino, principalmente com o uso de cartões SD, esta verificação é crucial para a gestão correta dos arquivos. Alternativas para a biblioteca SD padrão incluem SdFat, que oferece mais funcionalidades e eficiência em certos casos. A implementação dessas verificações geralmente depende do sistema de arquivos do dispositivo de armazenamento e da API fornecida pela biblioteca em uso.

## Veja Também:
- Documentação oficial da biblioteca SD do Arduino: [Arduino - SD](https://www.arduino.cc/en/Reference/SD)
- Biblioteca SdFat para alternativa de gestão de arquivos em SD: [SdFat - GitHub](https://github.com/greiman/SdFat)

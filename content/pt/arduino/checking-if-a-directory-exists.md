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

## Porquê
Algumas vezes, precisamos verificar se um diretório existe ou não antes de executar uma determinada tarefa em nosso código Arduino. Isso é importante, por exemplo, quando precisamos armazenar dados em um diretório específico ou carregar um arquivo de um diretório específico.

## Como Fazer
Para verificar se um diretório existe, usamos a função `SD.exists (path)` da biblioteca SD do Arduino. Primeiro, incluímos a biblioteca SD em nosso código:

```Arduino
#include <SD.h>
```

Em seguida, definimos o caminho do diretório que queremos verificar:

```Arduino
String path = "/diretorio/exemplo/";
```

E, finalmente, usamos a função `SD.exists()` para verificar se o diretório existe ou não:

```Arduino
if (SD.exists(path)) {
    // faça algo se o diretório existir
} else {
    // faça algo se o diretório não existir
}
```

Por exemplo, podemos criar um novo diretório se o mesmo não existir:

```Arduino
String path = "/novo_diretorio/";

if (!SD.exists(path)) {
    SD.mkdir(path);
    Serial.println("Diretório criado com sucesso!");
}
```

## Mergulho Profundo
Quando usamos a função `SD.exists()` para verificar se um diretório existe, o caminho especificado deve ser absoluto, ou seja, deve incluir o nome do cartão SD e o diretório raiz. Por exemplo, para verificar se o diretório "exemplo" existe no cartão SD, o caminho deve ser "/SD/exemplo/".

Outro ponto importante é que a função `SD.exists()` só é capaz de verificar a existência de diretórios, não de arquivos.

## Veja Também
- [Documentação da biblioteca SD do Arduino](https://www.arduino.cc/en/Reference/SD)
- [Guia completo de uso de cartões SD com Arduino](https://learn.adafruit.com/adafruit-micro-sd-breakout-board-card-tutorial?view=all)
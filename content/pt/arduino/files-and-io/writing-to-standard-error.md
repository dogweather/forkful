---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:00.409067-07:00
description: "Como fazer: O Arduino n\xE3o diferencia nativamente entre a sa\xEDda\
  \ padr\xE3o e o erro padr\xE3o como os sistemas de computa\xE7\xE3o convencionais.\
  \ Tanto os m\xE9todos\u2026"
lastmod: '2024-03-13T22:44:46.855012-06:00'
model: gpt-4-0125-preview
summary: "O Arduino n\xE3o diferencia nativamente entre a sa\xEDda padr\xE3o e o erro\
  \ padr\xE3o como os sistemas de computa\xE7\xE3o convencionais."
title: "Escrevendo para o erro padr\xE3o"
weight: 25
---

## Como fazer:
O Arduino não diferencia nativamente entre a saída padrão e o erro padrão como os sistemas de computação convencionais. Tanto os métodos `Serial.print()` quanto `Serial.println()` escrevem na mesma saída serial, normalmente visualizada no Monitor Serial da IDE do Arduino. No entanto, podemos emular a escrita em stderr formatando especificamente as mensagens de erro ou direcionando-as para uma saída alternativa, como um arquivo em um cartão SD ou por uma conexão de rede.

Para emular stderr, você pode prefixar mensagens de erro com uma etiqueta como "ERRO:" para diferenciá-las no Monitor Serial:

```cpp
void setup() {
  Serial.begin(9600); // Inicia a comunicação serial em 9600 baud rate
}

void loop() {
  int result = someFunction();
  if (result == -1) {
    // Emulando stderr prefixando a mensagem de erro
    Serial.println("ERRO: A função falhou ao executar.");
  } else {
    Serial.println("A função foi executada com sucesso.");
  }
  delay(1000); // Espera um segundo antes de reiniciar o loop
}

int someFunction() {
  // Uma função fictícia que retorna -1 em caso de erro
  return -1;
}
```

Um exemplo de saída no Monitor Serial da IDE Arduino pode parecer com isso:

```
ERRO: A função falhou ao executar.
```

Para projetos que requerem uma abordagem mais sofisticada, incluindo a escrita em diferentes saídas físicas, o uso de bibliotecas de terceiros ou hardware adicional pode ser necessário. Por exemplo, registrar mensagens de erro em um cartão SD requer a biblioteca `SD`:

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin()) {
    Serial.println("ERRO: Falha na inicialização do cartão SD!");
    return;
  }
  
  myFile = SD.open("error.log", FILE_WRITE);
  if (myFile) {
    myFile.println("ERRO: A função falhou ao executar.");
    myFile.close(); // Certifique-se de fechar o arquivo para salvar o conteúdo
  } else {
    Serial.println("ERRO: Falha ao abrir error.log!");
  }
}

void loop() {
  // Seu código principal iria aqui
}
```

Com esta abordagem, você separa fisicamente a saída normal do programa e as mensagens de erro direcionando estas últimas para um arquivo `error.log` em um cartão SD, permitindo análises post-mortem sem confundir o canal de saída principal.

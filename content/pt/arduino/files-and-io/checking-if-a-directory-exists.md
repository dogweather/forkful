---
title:                "Verificando se um diretório existe"
date:                  2024-02-03T19:06:40.543879-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verificando se um diretório existe"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Quê & Porquê?
No contexto da programação Arduino, verificar se um diretório existe em um cartão SD ou módulo de armazenamento similar permite que você leia ou escreva arquivos sem erros. Esta operação é essencial para o registro de dados, gestão de configuração ou qualquer tarefa que requeira armazenamento de arquivos estruturado, garantindo confiabilidade e desempenho fluido nas suas aplicações.

## Como Fazer:
O Arduino não suporta nativamente operações complexas de sistema de arquivos logo de cara. No entanto, com o uso da biblioteca SD, que é parte do Arduino IDE padrão, você pode facilmente trabalhar com arquivos e diretórios. Para verificar se um diretório existe, primeiro você precisa inicializar o cartão SD e, em seguida, usar o método `exists()` da biblioteca SD.

Primeiro, inclua a biblioteca SD e declare o pino de seleção de chip:

```cpp
#include <SPI.h>
#include <SD.h>

const int chipSelect = 4; // Pino de seleção de chip para o módulo de cartão SD
```

Na sua função `setup()`, inicialize o cartão SD e verifique se o diretório existe:

```cpp
void setup() {
  Serial.begin(9600);
  
  if (!SD.begin(chipSelect)) {
    Serial.println("Falha na inicialização!");
    return;
  }

  // Verifique se o diretório existe
  if (SD.exists("/myDir")) {
    Serial.println("Diretório existe.");
  } else {
    Serial.println("Diretório não existe.");
  }
}
```
Na função `loop()`, você pode mantê-la vazia ou adicionar outros códigos operacionais conforme necessário:

```cpp
void loop() {
  // Código operacional ou mantido vazio
}
```

A saída de amostra ao executar o código será:

```
Diretório existe.
```
ou

```
Diretório não existe.
```

É importante garantir que o cartão SD esteja formatado corretamente e o caminho do diretório `/myDir` esteja alinhado com suas necessidades específicas. Esta verificação básica é a pedra angular para realizar operações mais complexas com arquivos e diretórios em cartões SD com Arduino.

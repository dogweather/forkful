---
title:    "Arduino: Lendo um arquivo de texto"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto com Arduino?

Ler um arquivo de texto com Arduino pode ser útil em diversas situações, como por exemplo, quando você precisa armazenar dados específicos para serem utilizados em seu projeto. Com a leitura de um arquivo de texto, você pode obter informações importantes sem precisar armazená-las diretamente no código, tornando-o mais dinâmico e fácil de modificar.

## Como ler um arquivo de texto com Arduino

Para ler um arquivo de texto com Arduino, primeiro é necessário definir o nome do arquivo e a sua localização no dispositivo de armazenamento, seja ele uma memória SD ou a própria memória interna do Arduino. Em seguida, é preciso abrir o arquivo utilizando a função `open()` e informar o modo de leitura. Por exemplo, se quisermos ler o arquivo em modo de leitura, utilizamos o parâmetro `READ`.

```Arduino
#include <SPI.h>
#include <SD.h> // caso seja necessário utilizar a memória SD

File file; // declaração da variável do tipo File

void setup() {
  // código de inicialização do SD, caso esteja sendo utilizado
  Serial.begin(9600); // inicia conexão serial para imprimir resultados na porta serial

  // abre o arquivo no modo de leitura
  file = open("arquivo.txt", FILE_READ);
}

void loop() {
  // lê cada linha do arquivo e imprime na porta serial
  while (file.available()) {
    Serial.println(file.readStringUntil('\n'));
  }
}
```

O exemplo acima lê um arquivo chamado "arquivo.txt" que está localizado na mesma pasta do código do Arduino. Ele lê cada linha do arquivo e imprime o seu conteúdo na porta serial.

## Detalhando a leitura de um arquivo de texto

Além do modo de leitura, existem outras opções que podem ser utilizadas ao ler um arquivo de texto. Por exemplo, o parâmetro `FILE_WRITE`, que permite a escrita no arquivo, e o parâmetro `FILE_APPEND`, que adiciona conteúdo ao final do arquivo.

Outra função útil é a `readStringUntil()`, que lê o arquivo até encontrar o caractere especificado, neste caso, o `\n` que representa uma quebra de linha. Com ela, é possível definir um separador para o conteúdo do arquivo, o que facilita a leitura e organização dos dados.

## Veja também

- [Guia de referência oficial do Arduino sobre a leitura de arquivos](https://www.arduino.cc/reference/en/language/functions/filesystem/fopen/)
- [Exemplo completo de leitura de arquivo com Arduino](https://create.arduino.cc/projecthub/mustafa-hani/simple-sd-card-reader-example-1fc2c5)
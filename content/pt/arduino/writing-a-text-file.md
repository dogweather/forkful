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

## Por que escrever um arquivo de texto no Arduino?

Se você está trabalhando em um projeto de Arduino que requer o armazenamento de dados, escrever em um arquivo de texto pode ser uma opção útil. Isso permite que você armazene e atualize informações facilmente, sem a necessidade de editar o código diretamente.

## Como fazer:

Para escrever um arquivo de texto no Arduino, você precisará das seguintes bibliotecas:

- SD.h: permite o acesso ao cartão SD.
- SPI.h: comunicação serial para cartão SD.

Em seguida, declare o objeto SD e, em seguida, inicialize-o na função setup():

```Arduino
#include <SD.h>
#include <SPI.h>

File myFile;

void setup() {
  SD.begin(10); // pino CS do cartão SD
}
```

Agora, você pode criar o arquivo de texto e escrever nele usando o objeto File e o método print():

```Arduino
myFile = SD.open("meu_arquivo.txt", FILE_WRITE); // abre o arquivo de texto

myFile.print("Olá, mundo!"); // escreve o texto

myFile.close(); // fecha o arquivo
```

Certifique-se de fechar o arquivo após escrever nele, para garantir que todos os dados sejam salvos corretamente.

## Aprofundando:

Você também pode usar o método println() para adicionar uma nova linha ao arquivo de texto a cada impressão:

```Arduino
myFile = SD.open("meu_arquivo.txt", FILE_WRITE);

myFile.println("Este é um texto na primeira linha.");
myFile.println("Este é um texto na segunda linha.");

myFile.close();
```

Além disso, é interessante notar que você pode usar variáveis do Arduino para escrever em um arquivo de texto, por exemplo:

```Arduino
int valor = 5;
myFile.println("O valor atual é " + String(valor));
```

Dessa forma, é possível armazenar dados dinamicamente no arquivo de texto.

## Veja também:

- [Tutorial da SparkFun sobre escrever e ler arquivos de texto no Arduino](https://learn.sparkfun.com/tutorials/working-with-sd-cards-all#how-to-write-create-an-sd-card-text-file)
- [Documentação oficial do Arduino sobre as bibliotecas SD e SPI](https://www.arduino.cc/en/Reference/SD)
- [Vídeo tutorial do canal SparkFun sobre como usar a biblioteca SD](https://www.youtube.com/watch?v=ZryIKANUdlI)
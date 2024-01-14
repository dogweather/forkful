---
title:                "Arduino: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto no Arduino?

Escrever um arquivo de texto no Arduino pode ser útil em diversas situações, como armazenar dados coletados por sensores ou criar um registro de atividades do seu projeto. Além disso, pode ser uma ótima maneira de aprender mais sobre programação e expandir seu conhecimento em Arduino.

## Como fazer isso no Arduino

Para escrever um arquivo de texto no Arduino, primeiro é preciso criar um objeto do tipo `File` e especificar o nome e o modo de operação do arquivo. Por exemplo, para criar um arquivo chamado "dados.txt" para escrita, o código seria:

```Arduino
File myFile = SD.open("dados.txt", FILE_WRITE);
```

Em seguida, é possível utilizar o método `println()` para escrever dados no arquivo. Por exemplo, podemos escrever o valor lido de um sensor de temperatura:

```Arduino
myFile.println(sensorTemperatura);
```

Por fim, é importante fechar o arquivo para garantir que os dados sejam gravados corretamente:

```Arduino
myFile.close();
```

## Aprofundando-se

É importante entender que o tamanho máximo de um arquivo de texto no Arduino é limitado pela memória disponível. Além disso, é possível utilizar diferentes modos de operação ao criar o arquivo, como leitura, escrita ou criação de um novo arquivo.

Para acessar e manipular o conteúdo de um arquivo de texto, podem ser utilizados métodos como `read()`, `seek()` e `flush()`. É possível também criar estruturas de dados mais complexas, como arrays e objetos, e salvar esses dados no arquivo de texto.

Um exemplo completo de como criar, escrever e ler um arquivo de texto pode ser encontrado [neste tutorial](https://www.arduino.cc/en/Tutorial/ReadWrite).

## Veja também

- [Tutorial oficial do Arduino sobre leitura e escrita em arquivos](https://www.arduino.cc/en/Tutorial/ReadWrite)
- [Guia de referência da biblioteca SD do Arduino](https://www.arduino.cc/en/Reference/SD)
- [Exemplos práticos de escrita em arquivos de texto com o Arduino](https://www.practicalarduino.com/projects/text-file-interface)
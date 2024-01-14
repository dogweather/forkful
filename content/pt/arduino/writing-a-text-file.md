---
title:                "Arduino: Escrevendo um arquivo de texto"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto com Arduino?

Escrever um arquivo de texto pode ser útil em várias situações, como armazenar dados importantes ou criar um registro das atividades do Arduino. Além disso, é uma ótima maneira de praticar suas habilidades de programação.

## Como fazer

Para escrever um arquivo de texto utilizando o Arduino, siga os seguintes passos:

1. Comece conectando o seu Arduino ao seu computador via USB.
2. Abra a IDE do Arduino e crie um novo sketch.
3. Digite o seguinte código dentro do `loop()` para criar um novo arquivo de texto:
```Arduino
File myFile = SD.open("meuArquivo.txt", FILE_WRITE);
```
4. Em seguida, adicione o seguinte código para escrever no arquivo:
```Arduino
if (myFile) {
  myFile.println("Olá, mundo!"); // Escreve uma linha no arquivo
  myFile.println("Este é um exemplo de arquivo de texto com Arduino.");
  myFile.close(); // Fecha o arquivo
}
```
5. Upload o sketch para o seu Arduino e abra o Monitor Serial para ver o resultado.

O arquivo `meuArquivo.txt` será criado na sua placa SD e poderá ser acessado e lido a partir do seu computador.

## Mergulho profundo

Além do uso básico de escrever um arquivo de texto, existem outras funções e bibliotecas que podem ser utilizadas no Arduino para um maior controle e personalização.

A biblioteca `SD.h` é utilizada para acessar o cartão SD da placa e contém funções como `open()`, `println()` e `close()` para escrevermos no arquivo. Também é possível utilizar a função `write()` para escrever um byte específico.

É importante lembrar de utilizar a função `flush()` para garantir que os dados sejam gravados corretamente no arquivo antes de fechá-lo.

## Veja também

- [Tutorial: Escrevendo um arquivo de texto com Arduino e SD Card](https://www.arduino.cc/en/Tutorial/LibraryExamples/WriteToFile)
- [Documentação oficial da biblioteca SD.h](https://www.arduino.cc/en/Reference/SD)
- [Vídeo: Como escrever um arquivo de texto com o Arduino](https://www.youtube.com/watch?v=vLSge68f6ds)
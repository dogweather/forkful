---
title:                "Arduino: Lendo um arquivo de texto"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto é importante para programação Arduino?

Se você é um entusiasta de programação Arduino, é importante que você saiba como ler um arquivo de texto usando o seu dispositivo. Isso permitirá que você armazene e recupere facilmente dados importantes para o seu projeto.

## Como fazer:

Para ler um arquivo de texto usando o Arduino, siga os seguintes passos:

1. Abra o seu programa Arduino IDE.
2. Crie uma variável do tipo File e atribua-a ao seu arquivo de texto usando a função `SD.open()`.
3. Use o comando `while(file.available())` para garantir que ainda há dados disponíveis no arquivo.
4. Use a função `readLine()` para ler uma linha do arquivo e armazená-la em uma string.
5. Converta a string em um tipo de dados adequado para o seu projeto.
6. Feche o arquivo usando a função `file.close()`.

Aqui está um exemplo de como ler um arquivo de texto chamado "dados.txt" que contém valores de temperatura:

```Arduino
#include <SD.h>

File dados;

void setup() {
  Serial.begin(9600);
  dados = SD.open("dados.txt");
  while (dados.available()) {
    String linha = dados.readLine();
    float temperatura = linha.toFloat();
    Serial.println(temperatura);
  }
  dados.close();
}

void loop() {

}
```

### Saída:

Ao usar o código acima, você verá os valores de temperatura impressos em sua porta serial. Isso mostra como é fácil e útil ler dados de um arquivo de texto no Arduino.

## Mergulho profundo:

Além do método `readLine()`, o Arduino também possui outras funções úteis para ler arquivos de texto, como `readBytes()` e `readString()`. Estes métodos oferecem mais flexibilidade para ler dados em diferentes formatos.

Também é importante mencionar que, ao trabalhar com arquivos de texto, é necessário seguir certas convenções de formatação para garantir que os dados sejam lidos corretamente. Por exemplo, cada valor deve estar em uma linha separada e não deve haver espaços em branco extras entre as linhas.

Compreender como ler um arquivo de texto é uma habilidade essencial para qualquer programador Arduino, pois permite que você armazene e acesse facilmente dados para o seu projeto.

## Veja também:

- [Tutorial do Arduino sobre leitura de arquivos](https://www.arduino.cc/en/Tutorial/ReadASCIIString)
- [Documentação oficial do Arduino sobre leitura de arquivos](https://www.arduino.cc/reference/en/language/functions/communication/serial/readline/)
- [Tutorial sobre como utilizar o módulo SD com Arduino](https://www.filipeflop.com/blog/gravando-e-lendo-dados-num-cartao-sd-usando-arduino/)
- [Mais exemplos de como ler arquivos usando o Arduino](http://www.instructables.com/id/Arduino-File-IO-and-Instructables-Robot/)
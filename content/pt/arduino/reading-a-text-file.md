---
title:    "Arduino: Lendo um arquivo de texto"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto em Arduino?

Ler um arquivo de texto pode ser extremamente útil em projetos de Arduino. Isso permite que você armazene e acesse informações importantes, como configurações, mensagens ou dados do usuário. Além disso, é possível até mesmo criar um menu para o seu projeto usando um arquivo de texto.

## Como ler um arquivo de texto em Arduino

Ler um arquivo de texto em Arduino pode ser feito de forma simples e eficiente. Primeiro, vamos criar um arquivo de texto no computador e salvá-lo com a extensão .txt. Em seguida, precisamos armazenar esse arquivo em um cartão SD ou em uma memória EEPROM. Aqui está um exemplo de código para ler um arquivo de texto em um cartão SD:

```Arduino
#include <SPI.h>
#include <SD.h>

File file; // criando uma variável para armazenar o arquivo

void setup() {
  SD.begin(10); // iniciando a comunicação com o cartão SD
}

void loop() {
  file = SD.open("arquivo.txt"); // abrindo o arquivo de texto
  while (file.available()) { // percorrendo o arquivo
    Serial.println(file.readStringUntil('\n')); // imprimir cada linha do arquivo
  }
  file.close(); // fechando o arquivo
  delay(10000); // adicionar um atraso para evitar a leitura repetida do arquivo
}
```

Ao usar o comando `readStringUntil()`, podemos especificar o caractere que é usado para separar as linhas no arquivo, neste caso, o caractere ‘\n’ que representa a quebra de linha.

## Aprofundando no assunto

Ler um arquivo de texto é uma forma bastante útil e eficaz de armazenar e acessar informações em projetos de Arduino. Além disso, é possível também escrever em um arquivo de texto. Para isso, basta usar o comando `File.write()` e especificar o que deseja escrever no arquivo.

É importante lembrar que, ao usar um cartão SD, é necessário formatá-lo em FAT16 ou FAT32 para que o Arduino possa lê-lo corretamente. E ao usar uma memória EEPROM, é preciso primeiro configurá-la com um gravador de EEPROM.

## Veja também

- Como criar e escrever em um arquivo de texto em Arduino: https://www.arduino.cc/en/Tutorial/FilesWrite
- Mais dicas sobre como ler e escrever em um arquivo de texto: https://www.arduino.cc/en/Tutorial/Files
- Introdução à leitura de arquivos em SD com Arduino: https://www.arduino.cc/en/Reference/SD
- Como formatar um cartão SD para ser usado com Arduino: https://www.arduino.cc/en/Reference/SDCardNotes

Com essas informações, você poderá facilmente ler e escrever em arquivos de texto em seus projetos de Arduino. Experimente e divirta-se!
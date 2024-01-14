---
title:                "Arduino: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto com Arduino?

Ler um arquivo de texto pode ser uma tarefa útil e interessante para quem está programando em Arduino. Isso permite que você armazene e acesse dados de forma mais estruturada, possibilitando a criação de projetos mais complexos. Além disso, ler um arquivo de texto pode ser uma forma de aprendizado sobre como manipular arquivos, algo que pode ser útil em outras áreas da programação.

## Como ler um arquivo de texto com Arduino?

Para ler um arquivo de texto com Arduino, é necessário seguir alguns passos simples. Primeiro, é preciso criar o arquivo texto no mesmo diretório do seu código Arduino. O arquivo deve ter extensão .txt e pode ser criado em um editor de texto simples, como o Bloco de Notas do Windows.

Em seguida, é necessário estabelecer a comunicação com o dispositivo de armazenamento do Arduino, geralmente uma SD card ou uma memória flash. Você pode usar a biblioteca <code>SD.h</code> ou <code>SPI.h</code> para isso, de acordo com o tipo de dispositivo que está usando.

Uma vez estabelecida a comunicação, é preciso abrir o arquivo de texto com o comando <code>File.open()</code>. Lembre-se de definir o modo de abertura como <code>FILE_READ</code> para que o arquivo seja aberto apenas para leitura.

Agora, é possível ler o arquivo linha por linha usando o comando <code>File.readStringUntil('\n')</code>. Isso vai armazenar em uma variável a string até o primeiro caractere de quebra de linha, o que significa que a próxima leitura será feita a partir da próxima linha do arquivo.

Para facilitar a leitura, é possível também separar os valores por meio de um caractere de separação, como uma vírgula, usando o comando <code>File.readStringUntil(',')</code>.

Confira abaixo um exemplo de código que lê um arquivo de texto com dados de temperatura e umidade e imprime os valores no monitor serial:

```
#include <SPI.h>  // Biblioteca para comunicação com dispositivos
#include <SD.h>   // Biblioteca para comunicação com SD card

File dataFile;    // Variável para armazenar o arquivo de dados

void setup() {
  Serial.begin(9600);    // Inicia comunicação serial
  if (!SD.begin(4)) {    // Inicia comunicação com o SD card no pino 4
    Serial.println("Erro ao acessar SD card");  // Imprime mensagem de erro caso a comunicação falhe
    return;
  }
  dataFile = SD.open("dados.txt", FILE_READ);    // Abre o arquivo de texto para leitura
  if (dataFile) {   // Verifica se o arquivo foi aberto com sucesso
    while (dataFile.available()) {   // Executa enquanto ainda houver dados a serem lidos
      String line = dataFile.readStringUntil('\n');   // Lê uma linha do arquivo
      Serial.println(line);   // Imprime a linha lida no monitor serial
    }
    dataFile.close();   // Fecha o arquivo
  }
  else {   // Se o arquivo não for encontrado ou não for possível abri-lo
    Serial.println("Erro ao abrir arquivo");   // Imprime mensagem de erro no monitor serial
  }
}

void loop() {
  // Nada a ser feito no loop
}

```

O exemplo acima pode ser adaptado de acordo com as suas necessidades e com o formato dos dados em seu arquivo de texto. Experimente!

## Aprofundando-se na leitura de arquivos de texto com Arduino

Ler arquivos de texto com Arduino pode ser ainda mais dinâmico e complexo. É possível, por exemplo, escrever nos arquivos também, permitindo que você armazene dados criados pelo próprio Arduino. Além disso, é possível utilizar outras bibliotecas e comandos para manipular os dados lidos, como as funções de conversão de string, <code>int()</code> e <code>float()</code>.

Outro recurso interessante é a possibilidade de criar e acessar pastas dentro do dispositivo de armazenamento do Arduino, organizando melhor os seus arquivos de texto e facilitando a leitura.

Leia a documentação das bibliotecas e explore as diferentes possibilidades de leitura e escrita de arquivos de texto com Arduino. Quanto mais você se aprofundar, mais possibilidades terá para criar projetos criativos
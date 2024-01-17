---
title:                "Lendo um arquivo de texto"
html_title:           "Arduino: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O Que e Porque?

Ler um arquivo de texto é a ação de acessar e visualizar o conteúdo de um arquivo que contém texto. Isso é comumente feito por programadores para ler informações armazenadas em um arquivo de configuração, banco de dados ou qualquer outro tipo de arquivo de texto.

## Como Fazer:

```Arduino

// Inclua a biblioteca SD para acessar o cartão SD
#include <SD.h>

// Crie um objeto do tipo File para armazenar o arquivo que será lido
File arquivo;

void setup() {
  // Inicie a comunicação serial com a taxa de transmissão de 9600 bits por segundo
  Serial.begin(9600);

  // Inicialize o cartão SD
  if (!SD.begin(4)) {
    // Caso o cartão SD não esteja funcionando, exiba uma mensagem de erro e pare a execução do programa
    Serial.println("Erro ao inicializar o cartão SD");
    return;
  }

  // Abra o arquivo de texto, passando seu nome como parâmetro
  arquivo = SD.open("arquivo.txt");

  // Verifique se o arquivo foi aberto com sucesso
  if (arquivo) {
    // Enquanto houver linhas para ler, imprima cada uma delas na Serial
    while (arquivo.available()) {
      Serial.write(arquivo.read());
    }
    
    // Feche o arquivo quando terminar de usar
    arquivo.close();
  } else {
    // Caso contrário, exiba uma mensagem de erro
    Serial.println("Erro ao abrir o arquivo");
  }
}

void loop() {
  // Não é necessário fazer nada aqui, pois todo o código é executado apenas no setup()
}
```

### Saída de exemplo:

```
Olá, mundo!
Este é um arquivo de texto de exemplo.
Vamos ler seu conteúdo com Arduino.
```

## Aprofundando-se:

Ler arquivos de texto é uma tarefa essencial para muitos programadores, especialmente em projetos que envolvem armazenar e acessar informações em dispositivos externos, como cartões SD. Existem outras maneiras de armazenar e acessar informações, como utilizando bancos de dados, mas a leitura de arquivos de texto é uma opção mais simples e direta que pode ser facilmente implementada em projetos com Arduino.

Para utilizar a biblioteca SD do Arduino, é necessário ter um módulo com suporte para cartão SD conectado ao dispositivo. Além disso, é importante lembrar que os arquivos de texto são limitados em relação ao tipo de informação que podem armazenar, já que não suportam recursos como formatação avançada ou armazenamento de dados binários.

## Veja Também:

- [Documentação oficial do Arduino sobre a biblioteca SD](https://www.arduino.cc/en/Reference/SD)
- [Exemplo de projeto utilizando a biblioteca SD para ler e gravar dados em um cartão SD](https://create.arduino.cc/projecthub/electropeak/read-and-write-data-to-sd-card-with-arduino-uno-724797)
- [Tutorial em vídeo sobre como utilizar a biblioteca SD](https://www.youtube.com/watch?v=tzEYgK7UKRU)
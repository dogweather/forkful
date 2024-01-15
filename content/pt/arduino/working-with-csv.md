---
title:                "Trabalhando com arquivos csv"
html_title:           "Arduino: Trabalhando com arquivos csv"
simple_title:         "Trabalhando com arquivos csv"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que trabalhar com CSV?

Fazer a leitura e gravação de dados é uma parte essencial de muitos projetos de Arduino. CSV (Comma-Separated Values) é um formato popular e de fácil uso para realizar essa tarefa. Ao trabalhar com CSV, você pode armazenar e acessar dados complexos de forma rápida e eficiente.

## Como Fazer:

Para começar a trabalhar com CSV em Arduino, você precisará primeiro criar uma tabela com os dados que deseja armazenar. Por exemplo, uma tabela de temperatura e umidade com as colunas "hora", "temperatura" e "umidade".

```Arduino
#include <SPI.h>
#include <SD.h> // Biblioteca necessária para trabalhar com cartões SD

// Definir os pinos para o leitor de cartão SD
#define SD_CHIP_SELECT 4
File dataFile; // Variável para salvar o arquivo de dados

void setup() {
  // Inicializar o leitor de cartão SD
  if (!SD.begin(SD_CHIP_SELECT)) {
    // caso ocorra um erro, imprimir mensagem de "Erro ao iniciar o cartão SD"
    return;
  }

  // Abrir o arquivo de dados para escrita
  dataFile = SD.open("dados.csv", FILE_WRITE);
  // Verificar se o arquivo foi aberto com sucesso
  if (dataFile) {
    // Adicionar os nomes das colunas na primeira linha
    dataFile.println("hora, temperatura, umidade");
    // Fechar o arquivo
    dataFile.close();
  } else {
    // caso ocorra um erro, imprimir mensagem de "Erro ao criar o arquivo de dados"
  }
}

void loop() {
  // Ler os valores de hora, temperatura e umidade
  int hora = // código para ler a hora
  float temperatura = // código para ler a temperatura
  float umidade = // código para ler a umidade

  // Abrir o arquivo de dados para adicionar as novas linhas
  dataFile = SD.open("dados.csv", FILE_WRITE);
  // Verificar se o arquivo foi aberto com sucesso
  if (dataFile) {
    // Adicionar as informações da leitura atual na próxima linha
    dataFile.println(hora + "," + temperatura + "," + umidade);
    // Fechar o arquivo
    dataFile.close();
  } else {
    // caso ocorra um erro, imprimir mensagem de "Erro ao abrir o arquivo de dados"
  }
  // Código para pausar a leitura a cada X segundos
  delay(1000);
}

```

O código acima criará um arquivo chamado "dados.csv" no seu cartão SD com uma linha de cabeçalho e a cada leitura de hora, temperatura e umidade, adicionar uma nova linha com esses valores separados por vírgulas.

## Mais informações:

Além da leitura e escrita de dados simples, você também pode trabalhar com dados mais complexos em formato CSV, como matrizes e estruturas. Ao utilizar a biblioteca ArduinoCSV, é possível fazer a leitura e escrita desses tipos de dados de forma simples e eficiente.

Para saber mais sobre o funcionamento da biblioteca e todas as suas funcionalidades, confira a documentação oficial: [https://github.com/vinmenn/ArduinoCSV](https://github.com/vinmenn/ArduinoCSV)

## Veja também:

- [https://www.arduino.cc/en/Tutorial/CSV](https://www.arduino.cc/en/Tutorial/CSV) - Tutorial oficial da Arduino sobre como trabalhar com CSV.
- [https://blog.arduino.cc/2019/08/02/using-arduino-to-read-and-write-csv-files/](https://blog.arduino.cc/2019/08/02/using-arduino-to-read-and-write-csv-files/) - Artigo do blog da Arduino sobre o uso de CSV em projetos.
- [https://www.maketecheasier.com/send-data-from-arduino-to-google-sheets/](https://www.maketecheasier.com/send-data-from-arduino-to-google-sheets/) - Guia passo a passo de como enviar dados do Arduino para o Google Sheets usando CSV.
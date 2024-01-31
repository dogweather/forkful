---
title:                "Trabalhando com CSV"
date:                  2024-01-19
html_title:           "Bash: Trabalhando com CSV"
simple_title:         "Trabalhando com CSV"

category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?

Trabalhar com CSV (Valores Separados por Vírgula) envolve manipular arquivos de texto que armazenam dados em formato tabular. Programadores usam arquivos CSV pela sua simplicidade e interoperabilidade entre sistemas e programas diversos.

## Como Fazer:

```Arduino
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  SD.begin(10);

  myFile = SD.open("dados.csv", FILE_WRITE);

  if (myFile) {
    myFile.println("ID,Nome,Score");
    myFile.println("1,Joao,500");
    myFile.println("2,Ana,600");
    myFile.println("3,Pedro,700");
    myFile.close();
  } else {
    Serial.println("Erro ao abrir o arquivo");
  }

  myFile = SD.open("dados.csv");
  if (myFile) {
    while (myFile.available()) {
      String data = myFile.readStringUntil('\n');
      Serial.println(data);
    }
    myFile.close();
  } else {
    Serial.println("Erro ao abrir o arquivo");
  }
}

void loop() {
  // não é necessário código aqui para este exemplo
}
```

Saída esperada no Serial Monitor:

```
ID,Nome,Score
1,Joao,500
2,Ana,600
3,Pedro,700
```

## Aprofundando

CSV começou sua jornada nos primeiros dias dos computadores pessoais. Graças à sua capacidade de ser aberto em uma variedade de softwares, incluindo planilhas eletrônicas e bancos de dados, tornou-se um formato padrão para troca de dados. Alternativas incluem JSON ou XML, que carregam informações adicionais sobre o formato dos dados, mas são mais complexos. Ao implementar CSV em Arduino, é importante considerar a memória limitada, significando que uma gestão cuidadosa de arquivos e dados é essencial.

## Veja Também

- Documentação da biblioteca SD no Arduino: https://www.arduino.cc/en/Reference/SD
- CSV no Wikipedia: https://pt.wikipedia.org/wiki/Comma-separated_values
- Tutorial sobre como usar arquivos CSV com Arduino: https://www.arduino.cc/en/Tutorial/LibraryExamples/ReadWriteCSVFile

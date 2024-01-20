---
title:                "Trabalhando com csv"
html_title:           "Arduino: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## O que é e Por quê?

CSV (Valores Separados por Vírgula) é uma forma de armazenar dados em um formato de tabela simples, onde cada linha representa um registro e cada coluna representa um campo desse registro. Programadores usam o formato CSV para armazenar dados de uma forma organizada e fácil de ler.

## Como fazer:

Para trabalhar com CSV no Arduino, é necessário primeiro incluir a biblioteca `SPI.h` e `SD.h`. Em seguida, é preciso criar um arquivo CSV no cartão SD, seguindo o formato:

```
Nome,Cidade,Idade
Alice,São Paulo,25
Bruno,Rio de Janeiro,30
Clara,Belo Horizonte,27
```

Em seguida, é necessário abrir o arquivo e ler linha por linha, utilizando os comandos `readString()` e `nextString()`, e armazenando os dados em variáveis. Por exemplo:

```
String nome, cidade;
int idade;

File arquivo = SD.open("dados.csv");
// lê a primeira linha
nome = arquivo.readStringUntil(',');
cidade = arquivo.readStringUntil(',');
idade = arquivo.readStringUntil('\n');
// lê a segunda linha
nome = arquivo.readStringUntil(',');
cidade = arquivo.readStringUntil(',');
idade = arquivo.readStringUntil('\n');
```

Ao imprimir os valores das variáveis, o resultado será:

```
Alice
São Paulo
25
Bruno
Rio de Janeiro
30
```

## Profundidade:

O formato CSV foi introduzido pela Microsoft na década de 80 e desde então, se tornou uma forma popular de armazenamento de dados. Além do Arduino, é utilizado em diversas linguagens de programação e softwares de planilha, como o Excel. Outra forma de armazenamento de dados em formato de tabela é o JSON, porém, é necessária uma biblioteca externa para trabalhar com ele no Arduino.

## Veja também:

- [Tutorial sobre CSV no Arduino](https://www.arduino.cc/en/Tutorial/LibraryExamples/ReadWrite)
- [Biblioteca do Arduino para leitura e escrita de arquivos em cartão SD](https://www.arduino.cc/en/Reference/SD)
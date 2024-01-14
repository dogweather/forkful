---
title:                "Arduino: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário para o Arduino?

Criar um arquivo temporário pode ser uma tarefa muito útil em programação, especialmente para as pessoas que trabalham com Arduino. Um arquivo temporário é um arquivo que é criado para armazenar informações temporárias, que não são necessárias para serem salvas permanentemente. Esses arquivos podem ser úteis para realizar cálculos complexos, armazenar dados temporários ou até mesmo testar uma nova função.

## Como criar um arquivo temporário com Arduino

```Arduino
#include <SPI.h>
#include <SD.h>
String nomeArquivo = "tempfile.txt";
void setup() {
  //inicializa o cartão SD
  Serial.begin(9600);
  if (!SD.begin()) {
    Serial.println("Erro ao iniciar o cartão SD");
    return;
  }
  Serial.println("Inicialização do cartão SD bem sucedida");
  // cria o arquivo temporário
  File arquivoTemporario = SD.open(nomeArquivo, FILE_WRITE);
  // verifica se o arquivo foi aberto corretamente
  if (arquivoTemporario) {
    Serial.println("Novo arquivo temporário criado");
    // escreve algumas informações no arquivo
    arquivoTemporario.println("Hello!");
    arquivoTemporario.println("Este é um arquivo temporário");
    arquivoTemporario.println("Criando com sucesso usando Arduino");
    // fecha o arquivo
    arquivoTemporario.close();
  } 
  else {
    Serial.println("Erro ao criar o arquivo temporário");
  }
}
void loop() {
  // não faz nada no loop principal
}
```

```
Inicialização do cartão SD bem sucedida
Novo arquivo temporário criado
```

Este pequeno código mostra como criar um arquivo temporário chamado "tempfile.txt" usando um módulo SD e escrevendo algumas informações nele. Lembre-se de alterar o nome do arquivo para o que melhor se adapta ao seu projeto.

## Aprofundando-se na criação de um arquivo temporário

Criar um arquivo temporário é uma tarefa simples com o Arduino, mas é importante entender por que e quando você pode precisar dela. Existem várias situações em que pode ser útil criar um arquivo temporário, como armazenar dados temporários para processar um algoritmo ou registrar informações de teste durante o desenvolvimento de um projeto. Além disso, é importante lembrar de sempre fechar o arquivo após a sua utilização, para evitar problemas de memória e corrupção de dados.

## Veja também

- [Tutorial de como criar um arquivo temporário com Arduino](https://www.arduino.cc/en/Tutorial/ReadWrite)
- [Documentação oficial da biblioteca SD para Arduino](https://www.arduino.cc/en/reference/SD)
- [Exemplo de projeto usando arquivo temporário com Arduino](https://create.arduino.cc/projecthub/Electronoobs/sd-oled-door-alert-with-arduino-5e331e)
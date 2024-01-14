---
title:                "Arduino: Criando um arquivo temporário."
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário com Arduino?

Criar um arquivo temporário pode ser útil em diversas situações, principalmente quando se deseja armazenar dados temporariamente durante a execução de um programa. Esses arquivos são apagados automaticamente após o uso, o que economiza espaço na memória do Arduino.

## Como criar um arquivo temporário com Arduino

Para criar um arquivo temporário com Arduino, primeiro precisamos utilizar a biblioteca "SD" que permite a comunicação com cartões SD. Em seguida, devemos escolher um nome para o arquivo temporário e definir o seu tamanho. Veja um exemplo de código:

```
#include <SD.h>             // Incluindo a biblioteca SD

File arquivo;               // Criando uma variável para o arquivo
int tamanho = 100;          // Definindo o tamanho do arquivo

void setup(){
    Serial.begin(9600);     // Inicializando a comunicação serial
    arquivo = SD.open("temp.txt", FILE_WRITE);    // Criando o arquivo temporário
    if (arquivo){           // Verificando se o arquivo foi criado com sucesso
        Serial.println("Arquivo temporário criado com sucesso!");
    }
    else {                  // Caso o arquivo não tenha sido criado
        Serial.println("Erro ao criar arquivo temporário!");
    }
}

void loop(){
    // Realize alguma tarefa com os dados a serem armazenados no arquivo temporário
    
    // Escreva os dados no arquivo
    arquivo.println("Dados a serem armazenados...");
    delay(1000);
    
    // Feche o arquivo
    arquivo.close();
}
```

## Aprofundando no assunto

Caso seja necessário criar um arquivo temporário com mais de uma variável, podemos utilizar a função "sprintf()" para formatar os dados antes de escrevê-los no arquivo. Essa função funciona de forma semelhante a um "printf()", onde podemos especificar o tipo de dado e a variável correspondente para formatá-lo. Veja um exemplo:

```
#include <SD.h>             // Incluindo a biblioteca SD

File arquivo;               // Criando uma variável para o arquivo
int valor1 = 10;            // Definindo as variáveis a serem armazenadas
float valor2 = 3.14;

void setup(){
    Serial.begin(9600);     // Inicializando a comunicação serial
    arquivo = SD.open("temp.txt", FILE_WRITE);    // Criando o arquivo temporário
    if (arquivo){           // Verificando se o arquivo foi criado com sucesso
        Serial.println("Arquivo temporário criado com sucesso!");
    }
    else {                  // Caso o arquivo não tenha sido criado
        Serial.println("Erro ao criar arquivo temporário!");
    }
}

void loop(){
    // Realize alguma tarefa com os dados a serem armazenados no arquivo temporário
    
    // Formate os dados antes de escrevê-los no arquivo
    char buffer[50];                    // Definindo um buffer para armazenar os dados formatados
    sprintf(buffer, "%d, %.2f", valor1, valor2);  // Formatando os dados na ordem: int, float
    arquivo.println(buffer);            // Escrevendo os dados no arquivo
    delay(1000);
    
    // Feche o arquivo
    arquivo.close();
}
```

## Veja também

- Documentação oficial da biblioteca SD para Arduino: https://www.arduino.cc/en/Reference/SD

- Exemplo de projeto utilizando arquivos temporários com Arduino: https://create.arduino.cc/projecthub/parambirs/arduino-data-logger-with-timestamp-996b6d
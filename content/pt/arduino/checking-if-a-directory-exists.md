---
title:                "Verificando se um diretório existe"
html_title:           "Arduino: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Verificar se um diretório existe é o processo pelo qual se confirma a presença de uma pasta específica num sistema de arquivos. Programadores fazem isto para evitar erros de tempo de execução, tais como 'FileNotFoundException', que ocorrem quando se tenta acessar um diretório inexistente.

## Como fazer:

Aqui está um exemplo de como verificar a existência de um diretório com Arduino:

```Arduino
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Inicialização falhou!");
    return;
  }
  if (SD.exists("/meu_diretorio")) {
    Serial.println("O diretório existe.");
  } else {
    Serial.println("O diretório não existe.");
  }
}

void loop() {
  // código vazio
}

```

Quando executado, o código acima irá imprimir "O diretório existe." se "/meu_diretorio" existir, caso contrário, imprimirá "O diretório não existe.".

## Mergulho Profundo:

Ao verificar a existência de um diretório, estamos a manter uma prática de programação defensiva, o que ajuda a mitigar possíveis problemas. Esta funcionalidade foi introduzida na primeira versão da biblioteca SD no Arduino.

Alternativas a este método envolvem a manipulação direta dos erros ao tentar acessar o arquivo, ou utilizar outras bibliotecas para a realização desta tarefa, dependendo do sistema de arquivos em uso.

Um detalhe de implementação interessante é que a função `SD.exists()` não só verifica a existência de diretórios, mas também de arquivos. Isto deve ser levado em conta ao utilizar esta função.

## Veja também:

Para informações detalhadas sobre o uso do SD no Arduino, verifique a documentação oficial do Arduino [aqui](https://www.arduino.cc/en/reference/SD).

Para uma discussão profunda sobre programação defensiva, visite este link: [Programming defensively](https://en.wikipedia.org/wiki/Defensive_programming).

Para uma visão geral sobre manipulação de erros em Arduino, dê uma olhada [aqui](https://www.arduino.cc/en/tutorial/tryCatch).
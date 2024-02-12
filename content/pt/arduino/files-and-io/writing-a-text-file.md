---
title:                "Escrevendo um arquivo de texto"
date:                  2024-02-03T19:27:51.275753-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escrevendo um arquivo de texto"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?
Escrever um arquivo de texto no Arduino envolve salvar dados em um arquivo em um cartão SD ou módulo de armazenamento semelhante, muitas vezes para fins de registro de dados. Os programadores fazem isso para registrar leituras de sensores, salvar configurações ou registrar eventos de aplicativos ao longo do tempo, tornando isso crucial para projetos que requerem análise de dados ou acompanhamento.

## Como Fazer:
Para escrever em um arquivo de texto em um cartão SD usando Arduino, você primeiro precisa incluir a biblioteca `SD.h`, que fornece as funções necessárias para interagir com cartões SD. Certifique-se de que sua placa Arduino esteja conectada a um módulo de cartão SD.

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  // Inicializa a comunicação serial a 9600 bits por segundo:
  Serial.begin(9600);
  
  // Verifica a inicialização do cartão SD
  if (!SD.begin(4)) {
    Serial.println("Inicialização falhou!");
    return;
  }
  Serial.println("Inicialização concluída.");
  
  // Abre o arquivo. Note que apenas um arquivo pode ser aberto por vez,
  // então você tem que fechar este antes de abrir outro.
  myFile = SD.open("test.txt", FILE_WRITE);
  
  // Se o arquivo foi aberto com sucesso, escreva nele:
  if (myFile) {
    Serial.print("Escrevendo em test.txt...");
    myFile.println("Testando escrita de arquivo de texto.");
    // Fecha o arquivo:
    myFile.close();
    Serial.println("concluído.");
  } else {
    // Se o arquivo não abriu, imprima um erro:
    Serial.println("Erro ao abrir test.txt");
  }
}

void loop() {
  // Nada acontece após a configuração
}
```

### Saída de Exemplo:
Quando você executa este código, o Monitor Serial do IDE do Arduino exibirá:
```
Inicialização concluída.
Escrevendo em test.txt...concluído.
```
Para verificar se os dados foram escritos corretamente, você pode remover o cartão SD do Arduino, inseri-lo em um computador e abrir o arquivo `test.txt` para ver a mensagem "Testando escrita de arquivo de texto."

Para projetos que requerem operações de arquivos mais avançadas ou processamento, considere explorar bibliotecas adicionais ou escrever funções personalizadas adaptadas às suas necessidades específicas.

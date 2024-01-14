---
title:    "Arduino: Escrevendo em erro padrão"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão no Arduino?

Escrever para o erro padrão no Arduino pode ajudar a identificar e corrigir problemas no seu código. Quando um programa encontra um erro, ele geralmente o imprime no "erro padrão", que é uma área de memória que armazena mensagens de erro. Ao escrever para o erro padrão, você pode visualizar essas mensagens e descobrir onde o problema está ocorrendo.

## Como fazer:

Para escrever para o erro padrão no Arduino, você precisará usar a função `Serial.print()` ou `Serial.println()`. Essas funções enviam dados para o monitor serial, que é uma ferramenta útil para visualizar as mensagens de erro.

```
Arduino pinMode(13, OUTPUT);

digitalWrite(13, HIGH);

if (digitalRead(13) != HIGH) {
  Serial.println("Houve um erro ao configurar o pino 13 como saída.");
  }
```

Neste exemplo, o código tenta definir o pino 13 como saída, mas verifica se o pino realmente foi alterado para o estado HIGH. Se a verificação falhar, uma mensagem de erro será escrita para o erro padrão usando `Serial.println()`.

Você também pode escrever variáveis ​​para o erro padrão para monitorar o valor delas durante a execução do código. Isso pode ser útil para identificar problemas de lógica no seu código.

## Mergulho mais profundo:

Existem várias maneiras de visualizar as mensagens de erro do erro padrão. A forma mais comum é usar o monitor serial no ambiente de desenvolvimento Arduino (IDE). Você pode acessá-lo clicando em "Ferramentas" e em "Monitor serial" no menu.

Outra opção é usar um módulo de display LCD para exibir mensagens de erro diretamente no seu circuito. Existem bibliotecas disponíveis para ajudar a simplificar o processo de comunicação entre o Arduino e o LCD.

Além disso, é possível salvar as mensagens de erro em um cartão SD para visualizá-las posteriormente ou enviar as mensagens para um aplicativo de monitoramento no seu computador.

## Veja também:

- [Documentação oficial do Arduino para escrever para o erro padrão](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Tutorial do Arduino sobre como usar o monitor serial](https://www.arduino.cc/en/Tutorial/SerialMonitor)
- [Biblioteca LiquidCrystal para usar com displays LCD](https://www.arduino.cc/en/Reference/LiquidCrystal)
- [Tutorial sobre como usar um módulo de cartão SD no Arduino](https://www.arduino.cc/en/Tutorial/Datalogger)
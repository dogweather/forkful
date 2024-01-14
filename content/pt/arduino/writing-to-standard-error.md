---
title:    "Arduino: Escrevendo para o erro padrão"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Por que escrever para o erro padrão no Arduino

Escrever para o erro padrão, também conhecido como standard error, pode ser uma ferramenta útil para desenvolvedores que desejam analisar e depurar seus códigos no Arduino. Ao imprimir mensagens de erro ou informações de status no erro padrão, é possível identificar problemas e acompanhar o processo de execução do código.

# Como fazer

Para escrever no erro padrão no Arduino, é necessário utilizar a função `Serial.println()` para imprimir os dados desejados. É importante lembrar de iniciar a comunicação serial no setup do código, utilizando a função `Serial.begin()`. Veja um exemplo abaixo:

```arduino
// Iniciar a comunicação serial com baud rate de 9600
Serial.begin(9600);

// Imprimir mensagem no erro padrão
Serial.println("Este é um exemplo de mensagem de erro");
```

O código acima irá imprimir a mensagem "Este é um exemplo de mensagem de erro" no monitor serial do Arduino. É possível também utilizar a função `Serial.print()` para imprimir dados sem pular para uma nova linha.

# Mergulho profundo

Além de imprimir mensagens de erro, é possível utilizar a escrita no erro padrão para fins de debug ou monitoramento de variáveis durante a execução do código. Por exemplo, é possível imprimir o valor de uma variável em um determinado ponto do código para verificar se está sendo atribuída corretamente.

```arduino
int ledPin = 13;
int buttonPin = 2;

void setup() {
  // Iniciar a comunicação serial com baud rate de 9600
  Serial.begin(9600);
}

void loop() {
  // Ler o estado do botão
  int buttonState = digitalRead(buttonPin);

  // Imprimir o estado do botão no erro padrão
  Serial.println(buttonState);

  if (buttonState == HIGH) {
    // Acender o LED
    digitalWrite(ledPin, HIGH);
  } else {
    // Apagar o LED
    digitalWrite(ledPin, LOW);
  }

  // Aguardar 500ms
  delay(500);
}
```

No exemplo acima, a cada ciclo do loop, o estado do botão é impresso no erro padrão. Isso permite verificar se o botão está sendo lido corretamente e auxiliar no debugging do código.

# Veja também

- [Documentação oficial do Arduino sobre a comunicação serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Tutorial sobre o uso do erro padrão no Arduino](https://create.arduino.cc/projecthub/Aritro/implementing-error-handling-into-an-arduino-sketch-5001a2)
- [Vídeo explicativo sobre a comunicação serial no Arduino](https://www.youtube.com/watch?v=kpvbOzVWoug)
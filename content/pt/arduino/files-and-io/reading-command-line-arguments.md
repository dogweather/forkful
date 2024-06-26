---
date: 2024-01-20 17:55:26.040114-07:00
description: "Como Fazer: Arduino n\xE3o l\xEA argumentos da linha de comando diretamente,\
  \ pois n\xE3o tem um ambiente de terminal tradicional. Mas podemos simular algo\
  \ parecido\u2026"
lastmod: '2024-03-13T22:44:46.854039-06:00'
model: gpt-4-1106-preview
summary: "Arduino n\xE3o l\xEA argumentos da linha de comando diretamente, pois n\xE3\
  o tem um ambiente de terminal tradicional."
title: Lendo argumentos da linha de comando
weight: 23
---

## Como Fazer:
Arduino não lê argumentos da linha de comando diretamente, pois não tem um ambiente de terminal tradicional. Mas podemos simular algo parecido através da comunicação serial. Aqui está um exemplo básico:

```Arduino
void setup() {
  Serial.begin(9600); // Inicia a comunicação serial
  while (!Serial);    // Espera a porta serial conectar
}

void loop() {
  if (Serial.available() > 0) {                // Checa se há dados na porta serial
    String command = Serial.readStringUntil('\n'); // Lê a linha de comando
    Serial.print("Comando recebido: ");
    Serial.println(command);
  }
}
```

Envie comandos pela Serial e veja o que acontece:
```
> LED_ON
Comando recebido: LED_ON
```

## Mais Detalhes:
Como as placas Arduino não têm um sistema operacional com terminal, a comparação não é diretamente aplicável. Em máquinas típicas, argumentos da linha de comando são usados ​​para influenciar a execução de programas pelo terminal ou prompt de comando. No mundo Arduino, simulamos isso usando a comunicação serial, onde podemos enviar comandos para a placa através da porta USB e da IDE do Arduino ou outro monitor serial. Outras abordagens seriam usar botões físicos ou sensores para receber as instruções, embora não seja exatamente "linha de comando".

## Veja Também:
Para mais informações sobre comunicação serial no Arduino, acesse os seguintes links:

- Documentação oficial da função Serial: https://www.arduino.cc/reference/en/language/functions/communication/serial/
- Tutorial de comunicação Serial no Arduino: https://www.arduino.cc/en/Tutorial/BuiltInExamples/SerialEvent
- Forum Arduino para discussões sobre comandos e comunicação: https://forum.arduino.cc/

---
title:                "Exibindo saídas de depuração"
aliases:
- /pt/arduino/printing-debug-output.md
date:                  2024-01-20T17:51:46.623163-07:00
model:                 gpt-4-1106-preview
simple_title:         "Exibindo saídas de depuração"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?
Debugar escrevendo saídas é como dar uma espiada no cérebro do seu Arduino – você usa mensagens para descobrir o que está acontecendo dentro do código. Programadores fazem isso para corrigir erros e entender melhor o comportamento do programa.

## Como Fazer:
Para mostrar como isso funciona, vamos usar a função `Serial.println()`. Não esqueça de iniciar a comunicação serial no `setup()` com `Serial.begin()`.

```arduino
void setup() {
  // Inicia a comunicação serial na velocidade 9600 bits por segundo
  Serial.begin(9600);
}

void loop() {
  // Escreve "Olá, mundo!" na janela do Serial Monitor
  Serial.println("Olá, mundo!");
  
  // Espera um segundo para enviar novamente
  delay(1000);
}
```

Saída esperada no Serial Monitor:
```
Olá, mundo!
Olá, mundo!
Olá, mundo!
```
Repetido a cada segundo.

## Aprofundamento:
Antes do Arduino, debugar hardware era bem mais complicado e geralmente exigia equipamentos caros. A simplicidade de `Serial.print()` revolucionou a forma como fazemos debug nos projetos DIY. Existem alternativas, como o uso de displays LCD ou LEDs para indicar o status, mas a saída serial é geralmente a mais direta e informativa.

Quando implementa a saída de debug, pense no impacto no desempenho e tente evitar a sobrecarga na comunicação serial, especialmente em taxas de transmissão mais baixas.

## Veja Também:
- Arduino Reference for `Serial`: https://www.arduino.cc/reference/en/language/functions/communication/serial/
- Blog da Arduino sobre técnicas de debug: https://blog.arduino.cc/2021/08/06/debugging-your-arduino-sketches/
- Guia sobre otimização de código Arduino: https://www.arduino.cc/en/Tutorial/Foundations/CodeOptimization

---
title:                "Usando um depurador"
aliases:
- /pt/arduino/using-a-debugger.md
date:                  2024-01-26T03:47:21.430683-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando um depurador"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/using-a-debugger.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Um depurador é uma ferramenta que ajuda você a eliminar bugs no seu código, permitindo pausar, investigar e descobrir o que realmente está acontecendo por trás dos panos. Programadores usam depuradores para avançar passo a passo pelo código, inspecionar variáveis e entender onde as coisas podem estar dando errado.

## Como Fazer:

Com a IDE do Arduino, você pode usar impressões Serial para depurar, mas é um pouco como usar uma lanterna para explorar uma caverna. Para uma depuração de verdade, você pode querer elevar o nível com algo como o depurador Atmel-ICE, que se integra ao ambiente Arduino. Aqui está um gostinho da pseudo-depuração usando Serial:

```Arduino
void setup() {
  Serial.begin(9600);
}
void loop() {
  int sensorValue = analogRead(A0);
  Serial.print("Valor do Sensor: ");
  Serial.println(sensorValue);
  // Imagine que você está esperando 512 aqui, mas recebe 0.
  // Hora de inspecionar a conexão do sensor
  delay(1000); // Aguarde um segundo antes de ler novamente
}
```
Execute isso com o Monitor Serial aberto, e você verá o que seu sensor emite em tempo real.

## Mergulho Profundo

Antes dos depuradores, era o mundo das declarações de impressão - você só podia adivinhar o que estava acontecendo imprimindo tudo. Depurar com impressões ainda é comum, especialmente em ambientes mais simples ou em hardware limitado como o Arduino.

Alternativas a emuladores em circuito como o Atmel-ICE incluem ferramentas de depuração de software como `avr-gdb`. Você pode combiná-lo com `avarice` para criar uma ponte entre o GDB e seu hardware, o que é super útil para uma depuração mais avançada diretamente no chip.

Usando um depurador, você pode definir pontos de interrupção para parar a execução em certos pontos. Você pode avançar pelo seu código linha por linha, inspecionar memória, registros e variáveis. Isso permite que você identifique problemas ao invés de fazer tentativas no escuro. Ao implementar um depurador, garanta que seu ambiente esteja configurado corretamente - versões incompatíveis ou ferramentas mal configuradas podem levar a frustração.

## Veja Também

Pronto para aprofundar? Mergulhe nestes:
- O guia de depuração do Arduino em [Depuração Arduino](https://www.arduino.cc/en/Guide/Environment#toc7)
- O manual de referência AVR Libc para configurar avr-gdb: [Página Inicial AVR Libc](http://www.nongnu.org/avr-libc/)

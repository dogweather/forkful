---
title:                "Organizando o código em funções"
aliases:
- /pt/arduino/organizing-code-into-functions/
date:                  2024-01-26T01:08:58.918846-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizando o código em funções"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Organizar o código em funções significa dividir seu código em blocos reutilizáveis, cada um realizando um trabalho específico. Os programadores fazem isso para facilitar a leitura, depuração e reutilização do código. É como organizar Legos em caixas - isso evita que você revire uma pilha caótica toda vez que quiser construir algo.

## Como fazer:
Imagine que você queira fazer um LED piscar. Sem funções, seu `loop` é uma confusão desordenada. Com funções, é organizado. Veja como:

```Arduino
const int LED_PIN = 13;

void setup() {
  pinMode(LED_PIN, OUTPUT);
}

void loop() {
  blinkLED(500); // Piscar o LED a cada 500ms
}

// Função para piscar um LED
void blinkLED(int delayTime) {
  digitalWrite(LED_PIN, HIGH);
  delay(delayTime);
  digitalWrite(LED_PIN, LOW);
  delay(delayTime);
}
```

Saída de amostra: Seu LED está piscando alegremente, e a finalidade do código é clara à primeira vista.

## Mergulho Profundo
Antes das funções, a programação era como uma viagem de carro linear; você via cada buraco do início ao fim. Depois das funções, é mais como pegar voos - você pula para as partes importantes. Historicamente, as sub-rotinas (funções iniciais) foram uma revolução na programação, permitindo aos programadores evitar repetições – esse é o princípio DRY, Don't Repeat Yourself (Não Se Repita). Alternativas para funções podem incluir macros ou o uso de classes para programação orientada a objetos (OOP). O cerne da questão? Quando você define uma função, está dando ao compilador um plano para executar uma tarefa. Com o Arduino, você costuma definir funções void que atuam como comandos simples para um microcontrolador, mas funções também podem retornar valores, tornando-as mais versáteis.

## Veja Também
Para mais informações sobre funções, explore estes:

- Referência oficial de funções do Arduino: https://www.arduino.cc/reference/en/language/functions/
- Saiba mais sobre o princípio DRY: https://en.wikipedia.org/wiki/Don%27t_repeat_yourself
- Uma revisão sobre a história das sub-rotinas: https://en.wikipedia.org/wiki/Subroutine

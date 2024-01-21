---
title:                "Gerando números aleatórios"
date:                  2024-01-20T17:48:52.849046-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gerando números aleatórios"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O que é & Por quê?
Gerar números aleatórios é criar valores imprevisíveis e não-determinísticos. Programadores fazem isso para dar uma pitada de aleatoriedade e comportamento imprevisível em projetos, como em jogos ou simulações.

## Como Fazer:
```Arduino
void setup() {
  Serial.begin(9600);
  randomSeed(analogRead(0)); // Inicializa o gerador de números aleatórios
}

void loop() {
  int numeroAleatorio = random(1, 100); // Gera número entre 1 e 99
  Serial.println(numeroAleatorio);
  delay(1000); // Aguarda 1 segundo para o próximo número
}
```
Saída de amostra:
```
23
57
89
42
...
```

## Aprofundando
Historicamente, a função `random` da Arduino veio da necessidade de simulação de eventos aleatórios no mundo físico. Não é verdadeiramente aleatória; é baseada em um algoritmo - conhecido como gerador de números pseudoaleatórios (PRNG). A qualidade da aleatoriedade depende fortemente de uma boa "semente" inicial, que é onde `randomSeed` entra. Alternativas para maior aleatoriedade incluem o uso de sensores com ruído natural ou serviços web que fornecem números verdadeiramente aleatórios. Em sistemas mais complexos, considera-se o uso de algoritmos como Mersenne Twister ou algoritmos criptograficamente seguros para uma aleatoriedade robusta.

## Veja Também
- Documentação oficial da Arduino sobre a função `random()`: [Arduino - Random](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- Um artigo sobre a geração de números aleatórios e suas aplicações: [Generating Random Numbers: Everything You Need to Know](https://www.random.org/randomness/)
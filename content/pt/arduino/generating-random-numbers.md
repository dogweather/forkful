---
title:                "Gerando números aleatórios"
html_title:           "C: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Gerando Números Aleatórios no Arduino

## O que e Por quê?

Gerar números aleatórios significa criar números que não possuem nenhuma relação previsível entre si. Os programadores fazem isso para adicionar imprevisibilidade e variedade a um programa, jogo, experiência do usuário, etc.

## Como Fazer:

Vamos começar com um simples exemplo de como utilizar a função `random()` no Arduino.

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  int numero = random(0, 100); // Gera número aleatório entre 0 e 100.
  Serial.println(numero);
  delay(1000); // Espera por 1 segundo
}
```

O código acima imprime um número aleatório entre 0 e 100 a cada segundo.

## Mergulho Profundo:

A função `random()` no Arduino usa um gerador de números pseudorandom, que na verdade não são verdadeiramente aleatórios. Eles seguem um padrão predefinido que parece aleatório se você não sabe o padrão específico. 

Há uma alternativa que é usar uma combinação de leituras de sensores ou ruído de rádio para gerar números realmente aleatórios. 

Na implementação da função `random()`, o Arduino usa a função C `rand()`, que é implementado como um Gerador Linear Congruencial.

## Veja Também:

- Documentação oficial do Arduino para a função `random()`: [https://www.arduino.cc/en/Reference/random](https://www.arduino.cc/en/Reference/random)
- Detalhes técnicos sobre Geradores Lineares Congruenciais: [https://en.wikipedia.org/wiki/Linear_congruential_generator](https://en.wikipedia.org/wiki/Linear_congruential_generator)
---
date: 2024-01-27 20:32:40.956797-07:00
description: "Gerar n\xFAmeros aleat\xF3rios em projetos Arduino envolve produzir\
  \ valores que s\xE3o imprevis\xEDveis por design, crucial para aplica\xE7\xF5es\
  \ como jogos, simula\xE7\xF5es e\u2026"
lastmod: '2024-02-25T18:49:44.453181-07:00'
model: gpt-4-0125-preview
summary: "Gerar n\xFAmeros aleat\xF3rios em projetos Arduino envolve produzir valores\
  \ que s\xE3o imprevis\xEDveis por design, crucial para aplica\xE7\xF5es como jogos,\
  \ simula\xE7\xF5es e\u2026"
title: "Gera\xE7\xE3o de n\xFAmeros aleat\xF3rios"
---

{{< edit_this_page >}}

## O Que e Por Quê?
Gerar números aleatórios em projetos Arduino envolve produzir valores que são imprevisíveis por design, crucial para aplicações como jogos, simulações e sistemas de segurança. Os programadores utilizam essa técnica para introduzir variabilidade ou fazer decisões que não devem ser determinísticas.

## Como fazer:
O Arduino oferece funções simples para a geração de números aleatórios: `randomSeed()` e `random()`. Para começar, é necessário semear o gerador de números aleatórios para garantir diferentes sequências de números cada vez que seu programa é executado. Uma abordagem frequentemente usada é semear com uma leitura analógica de um pino não conectado.

```Arduino
void setup() {
  Serial.begin(9600);
  // Inicializar a semente aleatória
  randomSeed(analogRead(0));
}

void loop() {
  // Gerar um número aleatório entre 0 e 99
  int randomNumber = random(100);
  Serial.println(randomNumber);
  delay(1000); // Atraso de um segundo para legibilidade da saída
}
```

O programa acima inicializa o gerador de números aleatórios na função `setup()` e gera um novo número entre 0 e 99 em cada iteração do loop, exibindo o número no Monitor Serial.

Exemplo de saída:
```
42
17
93
...
```

## Aprofundamento
A função `random()` do Arduino, por debaixo dos panos, aproveita um gerador de números pseudoaleatórios (PRNG), que segue uma sequência determinística mas parece estatisticamente aleatória. O valor inicial, ou semente, da sequência influencia fortemente sua imprevisibilidade, daí o uso comum de `randomSeed()` com uma entrada um tanto quanto aleatória como ponto de partida. É importante notar que a aleatoriedade gerada pelo Arduino é suficiente para a maioria dos projetos de hobbistas, mas pode não atender aos critérios para aplicações de alta segurança devido à sua previsibilidade ao longo do tempo. Para fins criptográficos, é recomendável investigar algoritmos mais sofisticados e geradores de números aleatórios de hardware (HRNGs), que podem fornecer verdadeira aleatoriedade ao utilizar processos físicos.

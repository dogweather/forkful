---
title:                "Gerando números aleatórios"
html_title:           "Arduino: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

# O que & Por quê?

Gerar números aleatórios é uma função muito útil em programação, pois permite a criação de valores imprevisíveis para diversas aplicações, como jogos, criptografia e simulações. Os programadores utilizam essa funcionalidade para adicionar uma camada de imprevisibilidade aos seus códigos e torná-los mais interessantes e seguros.

# Como fazer:

Para gerar números aleatórios em um código Arduino, você pode utilizar a função random(). Ela recebe dois parâmetros: o valor mínimo e máximo que você deseja gerar. Por exemplo, se você quiser gerar um número entre 1 e 10, seu código ficaria assim:

```
random(1, 10);
```
A cada execução do código, um número aleatório entre 1 e 10 será gerado.

Você também pode utilizar a função randomSeed() para definir uma semente para a geração dos números aleatórios, o que garante que a sequência de números seja diferente a cada execução do código. Veja um exemplo:

```
randomSeed(analogRead(A0));
```

# Mergulho Profundo:

A geração de números aleatórios é um assunto muito importante em programação, principalmente na área de criptografia. No passado, as pessoas utilizavam métodos analógicos, como lançamentos de dados ou cartas, para gerar números aleatórios. Com o avanço da tecnologia e a criação de computadores, surgiram os geradores de números pseudoaleatórios, que utilizam algoritmos para criar sequências de números que parecem ser aleatórias.

No Arduino, a função random() também é baseada em um gerador de números pseudoaleatórios. Mas é importante ressaltar que esses números não são completamente aleatórios, pois seguem um padrão determinístico. Portanto, se a mesma semente for utilizada, a mesma sequência de números será gerada.

Existem outras alternativas para geração de números aleatórios no Arduino, como utilizar um circuito externo ou até mesmo um sensor de temperatura para obter valores mais imprevisíveis.

# Veja Também:

- Documentação oficial do Arduino sobre a função random(): https://www.arduino.cc/reference/en/language/functions/random-numbers/random/
- Tutorial da Adafruit sobre geradores de números aleatórios: https://learn.adafruit.com/random-numbers-in-arduino/overview
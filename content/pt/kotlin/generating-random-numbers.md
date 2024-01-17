---
title:                "Gerando números aleatórios"
html_title:           "Kotlin: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O que é e por que gerar números aleatórios?
Gerar números aleatórios é um processo no qual um programa de computador é usado para produzir um número que não segue nenhum padrão ou ordem específica. Os programadores usam isso para criar elementos de imprevisibilidade e variedade em seus programas.

## Como fazer:
```Kotlin
//Usando a função nextInt() do pacote Random para gerar um número inteiro aleatório entre 1 e 10
val randomInt = (1..10).random()

//Usando a função nextFloat() do pacote Random para gerar um número decimal aleatório entre 1.0 e 10.0
val randomFloat = Random.nextFloat() * 9 + 1
```

## Mergulho Profundo:
Historicamente, a geração de números aleatórios foi usada em jogos de azar, mas hoje em dia é amplamente utilizada em algoritmos de criptografia e simulações. Alternativas para gerar números aleatórios incluem usar valores de temperatura, pressão atmosférica ou o ruído de um microfone como fonte de imprevisibilidade. Na implementação, os números aleatórios são gerados por meio de equações matemáticas complexas, que são iniciadas com uma "semente" fornecida pelo sistema operacional.

## Veja também:
- Documentação oficial do pacote Random em Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/
- Explicação sobre a importância da qualidade dos números aleatórios em algoritmos de criptografia: https://www.journaldev.com/254/quality-of-random-numbers-cryptography
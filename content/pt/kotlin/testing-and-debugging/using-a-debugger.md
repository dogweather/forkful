---
date: 2024-01-26 03:50:10.781579-07:00
description: "Mergulhar em um depurador \xE9 tudo sobre percorrer seu c\xF3digo, observar\
  \ as engrenagens funcionando e pegar aqueles erros esquivos em flagrante. Os\u2026"
lastmod: '2024-03-11T00:14:20.251449-06:00'
model: gpt-4-0125-preview
summary: "Mergulhar em um depurador \xE9 tudo sobre percorrer seu c\xF3digo, observar\
  \ as engrenagens funcionando e pegar aqueles erros esquivos em flagrante. Os\u2026"
title: Usando um depurador
---

{{< edit_this_page >}}

## O Que & Por Quê?
Mergulhar em um depurador é tudo sobre percorrer seu código, observar as engrenagens funcionando e pegar aqueles erros esquivos em flagrante. Os programadores usam depuradores porque são as ferramentas de detetive que nos ajudam a descobrir onde as coisas dão errado sem arrancar os cabelos.

## Como Fazer:
Aqui vai uma pequena amostra de depuração em Kotlin com IntelliJ IDEA - o Sherlock Holmes dos IDEs:

```kotlin
fun main() {
    val mysteryNumber = 42
    var guess = 0

    while (guess != mysteryNumber) {
        println("Adivinhe o número: ")
        guess = readLine()?.toIntOrNull() ?: continue // Ignora entradas ruins

        // Define um ponto de interrupção aqui para observar 'guess' em ação
        if (guess < mysteryNumber) {
            println("Muito baixo!")
        } else if (guess > mysteryNumber) {
            println("Muito alto!")
        }
    }

    println("Você acertou! O número misterioso era $mysteryNumber")
}
```

Saída do depurador:
```
Adivinhe o número: 
10
Muito baixo!
Adivinhe o número: 
50
Muito alto!
Adivinhe o número: 
42
Você acertou! O número misterioso era 42
```

## Mergulho Profundo
Depuradores estão no jogo desde os anos 50. Naquela época, eles eram bem primitivos, e depurar poderia ser mais sobre hardware do que software. Hoje em dia, um depurador como o do IntelliJ IDEA nos permite definir pontos de interrupção, percorrer o código linha por linha e inspecionar o estado das variáveis a nosso bel-prazer.

Embora o depurador do IntelliJ seja super útil para Kotlin, ele não é o único peixe no mar. Há uma gama de alternativas como Logcat para desenvolvimento Android, ou ferramentas de linha de comando como jdb para os minimalistas. A mágica por baixo do capô aqui é principalmente sobre a Interface de Ferramenta da Máquina Virtual Java (JVMTI), que permite que os depuradores interajam com a Máquina Virtual Java, mantendo os desenvolvedores de Kotlin no loop.

## Veja Também
- Documentação do Depurador IntelliJ IDEA: [https://jetbrains.com/idea/](https://www.jetbrains.com/idea/features/debugger.html)

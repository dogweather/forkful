---
title:                "Lendo argumentos da linha de comando"
html_title:           "Kotlin: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O que e por que?

Ler argumentos da linha de comando é quando um programa recebe informações diretamente do terminal em que está sendo executado. Programadores fazem isso para tornar seus programas mais interativos e personalizáveis, permitindo que os usuários forneçam dados para o programa enquanto ele é executado.

## Como fazer:

```
fun main(args: Array<String>) {
    println("Olá, ${args[0]}!")
}
```

**Exemplo de entrada:**

```
kotlin MeuNome
```

**Saída:**

```
Olá, MeuNome!
```

## Aprofundamento:

- **Contexto histórico:** A habilidade de ler argumentos da linha de comando é algo essencial nas linguagens de programação desde o início. Isso permitiu que os programas fossem executados de forma diferente dependendo dos dados fornecidos pelos usuários.
- **Alternativas:** Usar um framework ou biblioteca externa para lidar com argumentos da linha de comando pode ser mais conveniente em alguns casos, mas também pode adicionar dependências extras ao seu projeto.
- **Detalhes de implementação:** Nas versões mais antigas do Kotlin (1.0 e 1.1), a classe `MainKt` era responsável por processar os argumentos da linha de comando. Na versão mais recente (1.3), a classe `ApplicationKt` é usada para esse propósito.

## Veja também:

- Documentação oficial do Kotlin sobre [leitura de argumentos de linha de comando](https://kotlinlang.org/docs/reference/properties.html#command-line-arguments).
- Um guia abrangente sobre a [sintaxe do Kotlin](https://kotlinlang.org/docs/reference/basic-syntax.html) para entender melhor como a linguagem funciona.
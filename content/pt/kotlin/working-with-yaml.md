---
title:                "Trabalhando com yaml"
html_title:           "Kotlin: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que usar YAML em projetos Kotlin?

YAML é uma linguagem de serialização de dados simples e legível por humanos, o que a torna uma ótima opção para trabalhar com configurações em projetos Kotlin. Com um formato intuitivo e flexível, YAML pode ajudar a simplificar o gerenciamento de configurações em seus projetos.

## Como usar YAML em projetos Kotlin

Usar YAML em projetos Kotlin é bastante simples. Primeiro, você precisará adicionar a dependência do YAML Parser em seu arquivo `build.gradle` ou `build.gradle.kts`:

```
// build.gradle
dependencies {
    implementation("com.charleskorn.kaml:kaml:0.20.0")
}
```

```
// build.gradle.kts
dependencies {
    implementation("com.charleskorn.kaml:kaml:0.20.0")
}
```

Em seguida, você pode usar o código a seguir para ler um arquivo YAML e obter seus dados:

```
import com.charleskorn.kaml.Yaml

fun main() {
    val yaml = Yaml.default.decodeFromString<T>(input)
    // faça algo com os dados YAML aqui
} 

```

E aqui está um exemplo de como você pode converter objetos Kotlin em YAML:

```
import com.charleskorn.kaml.Yaml

data class Pessoa(val nome: String, val idade: Int)

fun main() {
    val pessoa = Pessoa("João", 30)
    val yaml = Yaml.default.encodeToString(pessoa)
    println(yaml)
} 

```

Output:

```
nome: João
idade: 30
```

## Mergulho Profundo: Trabalhando com YAML em Projetos Kotlin

Uma das principais vantagens de usar YAML em projetos Kotlin é sua flexibilidade. Por exemplo, você pode facilmente adicionar comentários em seu arquivo YAML usando o símbolo `#`. Além disso, YAML também permite usar referências para evitar repetição de dados e ter um conjunto de dados mais organizado.

Outra funcionalidade útil é a possibilidade de usar chaves de sequência em YAML. Isso permite criar coleções de dados com chaves personalizadas e acessá-las facilmente em seu código Kotlin.

## Veja também

- [Site oficial do YAML](https://yaml.org/)
- [Documentação do Kaml para Kotlin](https://github.com/charleskorn/kaml)
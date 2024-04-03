---
date: 2024-01-26 04:23:48.474286-07:00
description: "Como Fazer: Para manipular TOML em Kotlin, voc\xEA pode usar uma biblioteca\
  \ como `ktoml`. Primeiro, vamos adicionar a depend\xEAncia no seu `build.gradle.kts`."
lastmod: '2024-03-13T22:44:46.565494-06:00'
model: gpt-4-0125-preview
summary: "Para manipular TOML em Kotlin, voc\xEA pode usar uma biblioteca como `ktoml`."
title: Trabalhando com TOML
weight: 39
---

## Como Fazer:
Para manipular TOML em Kotlin, você pode usar uma biblioteca como `ktoml`. Primeiro, vamos adicionar a dependência no seu `build.gradle.kts`:

```kotlin
dependencies {
    implementation("com.akuleshov7:ktoml:0.2.5")
}
```

Agora, vamos analisar algum TOML:

```kotlin
import com.akuleshov7.ktoml.file.TomlFileReader

fun main() {
    val conteudoToml = TomlFileReader.readAndParseFile("config.toml")
    
    val configuracaoBancoDados = conteudoToml.getTable("database")
    val host = configuracaoBancoDados.getString("host")
    val porta = configuracaoBancoDados.getLong("port")

    println("Host do Banco de Dados: $host")
    println("Porta do Banco de Dados: $porta")
}
```

Assumindo que `config.toml` se parece com isto:

```toml
[database]
host = "localhost"
port = 5432
```

A saída de amostra seria:

```
Host do Banco de Dados: localhost
Porta do Banco de Dados: 5432
```

## Aprofundamento
O TOML, criado pelo co-fundador do GitHub, Tom Preston-Werner, em 2013, visava ser mais direto que o YAML e mais seguro em termos de tipos do que o JSON. Tornou-se um sucesso, especialmente com o `Cargo` do Rust e o sistema de módulos do Go. Alternativas? YAML tem mais recursos, JSON traduz diretamente em objetos em muitas linguagens de programação, e sempre há o bom e velho XML. Quanto à implementação, ktoml, sob a licença Apache 2.0, é uma biblioteca puramente Kotlin e não traz bibliotecas Java junto, oferecendo DSLs para escrever TOML também, não apenas ler.

## Veja Também
- O GitHub do TOML: https://github.com/toml-lang/toml
- O GitHub do ktoml: https://github.com/akuleshov7/ktoml
- TOML vs. YAML vs. JSON: https://blog.logrocket.com/comparing-configuration-files-yaml-toml-json/

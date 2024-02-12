---
title:                "Trabalhando com TOML"
aliases: - /pt/kotlin/working-with-toml.md
date:                  2024-01-26T04:23:48.474286-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/working-with-toml.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
TOML significa Tom's Obvious, Minimal Language (Linguagem Óbvia e Mínima do Tom). É usado para arquivos de configuração porque é fácil de ler e escrever para humanos, enquanto ainda é fácil de analisar para máquinas. Os desenvolvedores optam por TOML para evitar o excesso de informações do XML e a complexidade do JSON quando estão manipulando configurações.

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

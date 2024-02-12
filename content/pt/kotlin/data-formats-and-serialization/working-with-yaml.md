---
title:                "Trabalhando com YAML"
aliases:
- /pt/kotlin/working-with-yaml.md
date:                  2024-02-03T19:26:06.249025-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?
YAML, que significa YAML Ain't Markup Language (YAML Não é Uma Linguagem de Marcação), é um formato de serialização de dados altamente legível, frequentemente utilizado para arquivos de configuração, armazenamento de dados e mensagens entre processos. Os programadores frequentemente trabalham com YAML para gerenciar configurações e ajustes de uma maneira estruturada, porém simples, se beneficiando de sua clareza e simplicidade em relação ao JSON ou XML quando a legibilidade é importante.

## Como Fazer:
Kotlin não possui suporte nativo para análise (parsing) e serialização de YAML, mas você pode utilizar bibliotecas de terceiros populares como `snakeyaml` (para análise de YAML em geral) e `kotlinx.serialization` (com uma extensão de formato YAML) para trabalhar com arquivos YAML.

### Usando `snakeyaml`
**Dependência:**
```kotlin
implementation 'org.yaml:snakeyaml:1.30'
```

**Ler YAML:**
```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.FileInputStream

fun readYaml(filePath: String) {
    val yaml = Yaml()
    val inputStream = FileInputStream(filePath)
    val data = yaml.load<Map<String, Any>>(inputStream)

    println(data)
}

// Uso de exemplo
fun main() {
    readYaml("config.yaml")
}
```
**Exemplo de `config.yaml`:**
```yaml
database:
  host: localhost
  port: 5432
```
**Saída de Exemplo:**
```
{database={host=localhost, port=5432}}
```
### Usando `kotlinx.serialization` com YAML
Primeiro, certifique-se de ter a biblioteca `kotlinx-serialization` com uma biblioteca de suporte YAML adequada (se disponível, já que `kotlinx.serialization` visa primariamente JSON e outros formatos diretamente).

**Dependência:**
```kotlin
// Para JSON (ilustrativo, verifique o suporte para YAML ou bibliotecas alternativas)
implementation 'org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.2'
```

**Definir uma classe de dados serializável:**
```kotlin
import kotlinx.serialization.Serializable

@Serializable
data class Config(
    val database: Database
)

@Serializable
data class Database(
    val host: String,
    val port: Int
)
```

Infelizmente, no momento da escrita, o suporte direto a YAML em `kotlinx.serialization` pode ser limitado ou estar evoluindo. Você pode precisar usar uma representação intermediária (como converter YAML para JSON com `snakeyaml` e depois analisar JSON com `kotlinx.serialization`) ou procurar por projetos de serialização de YAML impulsionados pela comunidade e compatíveis com `kotlinx.serialization`.

Para JSON, o código seria algo assim:
```kotlin
import kotlinx.serialization.json.Json
import kotlinx.serialization.decodeFromString

fun main() {
    val jsonText = """
    {
        "database": {
            "host": "localhost",
            "port": 5432
        }
    }
    """.trimIndent()
    
    val config = Json.decodeFromString<Config>(jsonText)
    println(config)
}
```

À medida que Kotlin e seu ecossistema continuam a evoluir, fique de olho na documentação oficial e nos recursos da comunidade para as últimas novidades sobre suporte a YAML e bibliotecas.

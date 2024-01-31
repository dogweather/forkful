---
title:                "Trabalhando com YAML"
date:                  2024-01-19
html_title:           "Arduino: Trabalhando com YAML"
simple_title:         "Trabalhando com YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML é um formato de serialização de dados legível por humanos, usado para configuração de aplicativos, integração entre serviços e armazenamento de dados. Programadores utilizam YAML pela simplicidade e facilidade de ler e escrever comparado a XML ou JSON.

## How to:
Em Kotlin, podemos usar a biblioteca `snakeyaml` para trabalhar com YAML. Primeiro, adicione a dependência no seu `build.gradle`:

```kotlin
dependencies {
    implementation("org.yaml:snakeyaml:1.29")
}
```

Agora, vamos ler e escrever YAML:

```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.FileInputStream
import java.io.FileWriter

fun main() {
    // Ler YAML
    val yaml = Yaml()
    val inputStream = FileInputStream("config.yaml")
    val data = yaml.load<Map<String, Any>>(inputStream)
    println(data)

    // Escrever YAML
    val outputData = mapOf("name" to "João", "idade" to 30)
    val writer = FileWriter("output.yaml")
    yaml.dump(outputData, writer)
}
```

Output `config.yaml` lido:
```yaml
nome: João
idade: 30
```

Output `output.yaml` escrito:
```yaml
idade: 30
nome: João
```

## Deep Dive
YAML, que significa "YAML Ain't Markup Language" (ou recursivamente "YAML Ain't Markup Language"), foi introduzido em 2001 como uma alternativa ao XML. Opções como JSON e TOML também são usadas, mas YAML é preferido quando legibilidade e comentários são importantes. Internamente, bibliotecas como `snakeyaml` convertem dados YAML para estruturas de dados nativas, como Mapas e Listas no Kotlin.

## See Also
- Documentação Oficial YAML: https://yaml.org/spec/1.2/spec.html
- SnakeYAML Engine GitHub: https://bitbucket.org/asomov/snakeyaml/src/master/
- Tutorial JSON em Kotlin para comparação: https://www.baeldung.com/kotlin/json

Lembre que ao trabalhar com YAML, atenção especial a indentação é crucial, pois a estrutura do arquivo depende disso. Boa codificação!

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

## O que é e por que fazer em YAML?

YAML é uma linguagem de marcação que é frequentemente usada por desenvolvedores para armazenar e transmitir dados estruturados. É uma alternativa leve e fácil de ler em comparação com outras linguagens de marcação, como XML e JSON.
Os programadores optam por trabalhar com YAML por sua simplicidade e flexibilidade, tornando mais fácil lidar com grandes quantidades de dados.

## Como fazer:

Para começar a trabalhar com YAML em Kotlin, é necessário importar a biblioteca SnakeYAML através do gerenciador de dependências Gradle. Depois disso, basta seguir os seguintes passos:

**1. Criando um arquivo YAML:** Para criar um arquivo YAML, basta criar uma variável do tipo `Map` com os dados desejados e usar o método `dump()` da biblioteca SnakeYAML para convertê-lo em YAML.

```
val dados = mapOf("nome" to "Maria", "idade" to 25, "cidade" to "São Paulo")
val yaml = Yaml().dump(dados)
println(yaml)
```
**Output:**
```
nome: Maria
idade: 25
cidade: São Paulo
```

**2. Lendo um arquivo YAML:** Para ler um arquivo YAML, basta usar o método `load()` da biblioteca SnakeYAML e especificar o caminho para o arquivo YAML.

```
val yaml = File("arquivo.yaml").readText()
val dados = Yaml().load<Map<String, Any>>(yaml)
println(dados)
```
**Output:**
```
{nome=Maria, idade=25, cidade=São Paulo}
```

## Mergulho Profundo:

YAML foi criado em 2001 por Clark Evans como uma alternativa mais fácil de usar para XML. Atualmente, é amplamente utilizado em projetos de desenvolvimento, especialmente em aplicações web. Algumas alternativas para YAML incluem TOML e JSON, mas YAML ainda é considerado o mais adequado para dados complexos e bem estruturados.

A biblioteca SnakeYAML é a principal opção para trabalhar com YAML em Kotlin. Ela fornece métodos para converter dados Java em YAML e vice-versa, além de oferecer suporte para recursos avançados, como referências e tipos personalizados.

## Saiba Mais:

- [Site oficial do YAML](https://yaml.org/)
- [Biblioteca SnakeYAML](https://bitbucket.org/asomov/snakeyaml/wiki/Documentation)
- [Tutorial de YAML em Kotlin](https://blog.kotlin-academy.com/working-with-yaml-in-kotlin-a-practical-tutorial-8f332fa88de4)
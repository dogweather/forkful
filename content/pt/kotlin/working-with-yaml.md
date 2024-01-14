---
title:                "Kotlin: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

##Por que trabalhar com YAML?

YAML é uma linguagem de marcação de dados simples e fácil de entender. É frequentemente usada para configurar informações em sistemas de computador, tornando-a uma ferramenta útil para desenvolvedores de software.

##Como fazer:

Para começar a trabalhar com YAML em Kotlin, primeiro precisamos importar a biblioteca de suporte YAML, adicionando a seguinte linha em nosso arquivo Gradle:

```
dependencies {
    implementation("io.github.mike10004:yamlq:$yamlq_version")
}
```

Agora, podemos criar um objeto YAMLQ e usá-lo para ler e gravar arquivos YAML:

```
val yamlq = YAMLQ()
//lendo um arquivo YAML
val input = File("arquivo.yaml")
val dados = yamlq.read(input)

//escrevendo um arquivo YAML
yamlq.write(data, File("arquivo2.yaml"))
```

##Mergulho Profundo:

Além de ler e gravar arquivos YAML, podemos usar as classes do pacote YAMLQ para fazer consultas mais avançadas nos dados. Por exemplo, podemos usar a classe `YAMLQPath` para navegar nos dados selecionando caminhos específicos. Vamos ver um exemplo:

```
val yamlq = YAMLQ()
val input = """
    nome: João
    idade: 25
    endereço:
        cidade: São Paulo
        país: Brasil
    amigos:
        - Maria
        - Pedro
        - Ana
""".reader()
val dados = yamlq.read(input)
val caminho = YAMLQPath("$.amigos[0]") //selecionando o primeiro elemento da lista amigos
val amigo = caminho.select(dados) //retornando o valor "Maria"
println(amigo)
```

Este é apenas um exemplo simples, mas há muitas outras funcionalidades interessantes que podem ser exploradas ao trabalhar com YAML em Kotlin.

##Veja também:

- [Documentação oficial do YAML](https://yaml.org/)
- [Repositório GitHub de YAMLQ](https://github.com/mike10004/yamlq)
- [Tutorial de introdução ao YAML em Kotlin](https://www.baeldung.com/kotlin-yaml)
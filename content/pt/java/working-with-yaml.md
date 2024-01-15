---
title:                "Trabalhando com yaml"
html_title:           "Java: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que
Há muitas razões pelas quais alguém pode se interessar em trabalhar com YAML em programação Java. YAML é uma linguagem de marcação utilizada para estruturar dados em um formato legível e fácil de escrever, o que a torna muito útil para configurações de aplicativos e troca de dados.

## Como fazer
Para começar a trabalhar com YAML em Java, você precisará adicionar a dependência correspondente ao seu projeto. Você pode fazer isso manualmente ou através de um gerenciador de dependências como Maven ou Gradle.

Uma vez que a dependência esteja adicionada, você pode criar um objeto YAML utilizando a biblioteca SnakeYAML. Aqui está um exemplo simples:

```
//Importando a biblioteca SnakeYAML
import org.yaml.snakeyaml.*;

//Criação do objeto YAML
Yaml yaml = new Yaml();

//Definindo o conteúdo do YAML como uma string
String yamlString = "nome: João\nidade: 30\nprofissão: programador";

//Lendo o YAML e armazenando em um mapa
Map<String, Object> yamlMap = yaml.load(yamlString);

//Imprimindo o conteúdo do mapa
System.out.println(yamlMap);
```

A saída deste código será: `{nome=João, idade=30, profissão=programador}`.

Além disso, também é possível ler e escrever arquivos YAML utilizando as classes `YamlReader` e `YamlWriter` do SnakeYAML. Você pode encontrar mais exemplos e informações na documentação oficial da biblioteca.

## Mergulho Profundo
Além de sua simplicidade e legibilidade, YAML também oferece recursos avançados para estruturar dados. Algumas características interessantes incluem:

- Aninhamento de dados: em YAML, é possível aninhar estruturas de dados, o que permite criar estruturas complexas e bem organizadas.
- Referências: com o uso de referências, é possível reutilizar dados em diferentes partes do seu YAML, tornando-o mais conciso e fácil de manter.
- Tipos personalizados: YAML permite definir tipos personalizados para representar dados, oferecendo mais flexibilidade e controle sobre a estrutura dos seus dados.

Como mencionado anteriormente, é possível encontrar mais informações e exemplos na documentação do SnakeYAML.

## Veja Também
- [Documentação oficial do SnakeYAML](https://bitbucket.org/asomov/snakeyaml/src/default/)
- [Tutorial de YAML](https://www.baeldung.com/java-snake-yaml)
- [Exemplo prático de uso de YAML em Java](https://dzone.com/articles/introduction-to-snakeyamls-java-api-for-loading-yaml)
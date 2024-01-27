---
title:                "Trabalhando com YAML"
date:                  2024-01-19
html_title:           "Arduino: Trabalhando com YAML"
simple_title:         "Trabalhando com YAML"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?

Trabalhar com YAML significa manipular dados em um formato que é humano-legível, facilitando a configuração e troca de dados entre serviços e programas. Programadores recorrem ao YAML por sua simplicidade e leitura fácil, muito comum em configurações de aplicações e desenvolvimento de software.

## Como Fazer:

Para manipular YAML em Java, você pode usar a biblioteca `SnakeYAML`. Vou mostrar como ler e escrever YAML.

**Ler YAML:**
```java
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.util.Map;

public class YamlReader {
    public static void main(String[] args) {
        // Carregar o arquivo YAML
        InputStream inputStream = YamlReader.class
            .getClassLoader()
            .getResourceAsStream("config.yaml");

        Yaml yaml = new Yaml();
        Map<String, Object> data = yaml.load(inputStream);
        System.out.println(data);
    }
}
```
**Escrever YAML:**
```java
import org.yaml.snakeyaml.Yaml;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class YamlWriter {
    public static void main(String[] args) {
        // Dados para serem escritos no YAML
        Map<String, Object> data = new HashMap<>();
        data.put("nome", "João");
        data.put("idade", 30);

        Yaml yaml = new Yaml();
        try (FileWriter writer = new FileWriter("output.yaml")) {
            yaml.dump(data, writer);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
**Saída do exemplo de leitura (supondo que `config.yaml` contém dados apropriados):**
```
{nome=João, idade=30}
```
**Saída do exemplo de escrita (conteúdo de `output.yaml`):**
```yaml
idade: 30
nome: João
```

## Aprofundamento:

YAML, acrônimo para "YAML Ain't Markup Language", surgiu em 2001 como uma alternativa mais legível e simples ao XML. Hoje, é frequentemente usado para arquivos Docker Compose, Kubernetes, e muitos outros sistemas de automação. Alternativas incluem JSON, que é mais utilizado para APIs e serviços web pela sua compatibilidade e velocidade de parsing, mas é menos legível para configurações complexas.

Ao usar YAML no Java, é crucial validar e sanitizar os dados para evitar vulnerabilidades como a injeção de YAML. Portanto, é recomendado trabalhar com bibliotecas bem mantidas, como a SnakeYAML, e estar ciente de como as estruturas são deserializadas.

## Veja Também:

- Especificação YAML: [https://yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)
- Tutorial YAML interativo: [https://learnxinyminutes.com/docs/yaml/](https://learnxinyminutes.com/docs/yaml/)

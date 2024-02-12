---
title:                "Trabalhando com YAML"
aliases:
- pt/java/working-with-yaml.md
date:                  2024-02-03T19:25:31.821717-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O que é & Por quê?
O YAML, abreviação de "YAML Ain't Markup Language" (YAML Não é uma Linguagem de Marcação), é um padrão de serialização de dados legível por humanos que os programadores utilizam para arquivos de configuração, dumping de dados e transmissão de dados entre linguagens. É popular devido à sua legibilidade e facilidade de uso, tornando-o uma escolha comum para a configuração de aplicações e serviços.

## Como fazer:
No Java, você pode trabalhar com arquivos YAML usando bibliotecas de terceiros, já que a Edição Padrão do Java não inclui suporte integrado para YAML. Uma biblioteca popular é a SnakeYAML, que permite a análise e geração de dados YAML facilmente.

### Configurando o SnakeYAML
Primeiro, inclua o SnakeYAML em seu projeto. Se estiver usando Maven, adicione a seguinte dependência ao seu `pom.xml`:

```xml
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.30</version>
</dependency>
```

### Lendo YAML
```java
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.util.Map;

public class ReadYamlExample {
    public static void main(String[] args) {
        Yaml yaml = new Yaml();
        try (InputStream inputStream = ReadYamlExample.class
                .getClassLoader()
                .getResourceAsStream("config.yml")) {
            Map<String, Object> data = yaml.load(inputStream);
            System.out.println(data);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```
Assumindo que `config.yml` seja assim:
```yaml
name: Example
version: 1.0
features:
  - login
  - signup
```
A saída será:
```
{name=Example, version=1.0, features=[login, signup]}
```

### Escrevendo YAML
Para gerar um YAML a partir de objetos Java, utilize o método `dump` fornecido pelo SnakeYAML:
```java
import org.yaml.snakeyaml.Yaml;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;

public class WriteYamlExample {
    public static void main(String[] args) {
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("name", "Example");
        data.put("version", 1.0);
        data.put("features", Arrays.asList("login", "signup"));

        Yaml yaml = new Yaml();
        String output = yaml.dump(data);
        System.out.println(output);
    }
}
```
Isso irá gerar e imprimir o seguinte conteúdo YAML:
```yaml
name: Example
version: 1.0
features:
- login
- signup
```
Ao aproveitar o SnakeYAML, os desenvolvedores Java podem integrar facilmente a análise e geração de YAML em suas aplicações, beneficiando-se da legibilidade e simplicidade do YAML para fins de configuração e troca de dados.

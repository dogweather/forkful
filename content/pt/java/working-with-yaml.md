---
title:                "Java: Trabalhando com YAML"
simple_title:         "Trabalhando com YAML"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que
O formato YAML tem se tornado cada vez mais popular na programação Java devido à sua simplicidade e facilidade de leitura. Com a sintaxe baseada em espaços, YAML é uma ótima escolha para armazenar e transferir dados estruturados.

## Como Fazer
Para trabalhar com YAML em Java, é necessário importar a biblioteca SnakeYAML. Você pode fazer isso adicionando a seguinte dependência ao seu arquivo pom.xml (caso esteja usando Maven):

```Java
<dependency>
  <groupId>org.yaml</groupId>
  <artifactId>snakeyaml</artifactId>
  <version>1.27</version>
</dependency>
```

Agora, você pode começar a usar a biblioteca em seu código. Veja um exemplo simples de como ler um arquivo YAML:

```Java
import org.yaml.snakeyaml.Yaml;

public class ExemploYAML {
    public static void main(String[] args) {
        Yaml yaml = new Yaml();
        Map<String, Object> dados = yaml.load(new FileInputStream("exemplo.yaml"));
        System.out.println(dados.get("nome"));
    }
}
```

Supondo que o conteúdo do arquivo `exemplo.yaml` seja:

```YAML
nome: João
idade: 25
cidade: São Paulo
```

A saída será: `João`.

Você também pode criar e escrever em arquivos YAML usando a biblioteca SnakeYAML. Veja um exemplo:

```Java
import org.yaml.snakeyaml.Yaml;

public class ExemploYAML {
    public static void main(String[] args) {
        Yaml yaml = new Yaml();
        Map<String, Object> dados = new HashMap<>();
        dados.put("nome", "Maria");
        dados.put("idade", 30);
        dados.put("cidade", "Rio de Janeiro");
        File arquivo = new File("dados.yaml");
        yaml.dump(dados, new FileWriter(arquivo));
    }
}
```

Isso criará um arquivo chamado `dados.yaml` com o seguinte conteúdo:

```YAML
nome: Maria
idade: 30
cidade: Rio de Janeiro
```

## Mergulho Profundo
Trabalhar com YAML em Java pode ser muito útil em diversas situações, como na leitura e escrita de configurações para uma aplicação ou na troca de dados com outras linguagens que também utilizam esse formato.

É importante notar que YAML em Java é representado por tipos de dados Java, como `Map`, `List` e `String`. Isso significa que você precisa estar familiarizado com esses tipos para trabalhar com YAML de forma eficiente.

Além disso, existem algumas funcionalidades avançadas na biblioteca SnakeYAML, como a capacidade de definir uma classe Java para mapear os dados de um arquivo YAML. Essas funcionalidades podem ser úteis para projetos mais complexos.

## Veja Também
- [Site oficial da biblioteca SnakeYAML](https://bitbucket.org/asomov/snakeyaml)
- [Tutorial para trabalhar com YAML em Java](https://www.baeldung.com/java-yaml)
- [Documentação do formato YAML](https://yaml.org/spec/1.2/spec.html)
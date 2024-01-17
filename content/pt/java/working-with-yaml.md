---
title:                "Trabalhando com YAML"
html_title:           "Java: Trabalhando com YAML"
simple_title:         "Trabalhando com YAML"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Trabalhar com YAML é uma forma de estruturar e armazenar dados de forma organizada em arquivos de texto. Os programadores utilizam YAML porque é uma maneira simples e legível para definir configurações e metadados em seus projetos.

## Como fazer:
Para começar a trabalhar com YAML em Java, é necessário adicionar uma biblioteca externa chamada "SnakeYAML" ao seu projeto. Use o código abaixo como exemplo:

```Java
import org.yaml.snakeyaml.Yaml;

public class Main {
    public static void main(String[] args) {
        // Convertendo um objeto para YAML
        User user = new User("John", "Doe", 25);
        Yaml yaml = new Yaml();
        String output = yaml.dump(user);
        System.out.println(output);

        // Convertendo YAML para objeto
        String yaml = "name: John\nlast name: Doe\nage: 25";
        User user = yaml.loadAs(yaml, User.class);
        System.out.println(user.getName());
    }

    // Classe de exemplo para conversão
    public class User {
        private String name;
        private String lastName;
        private int age;

        public User(String name, String lastName, int age) {
            this.name = name;
            this.lastName = lastName;
            this.age = age;
        }

        public String getName() {
            return name;
        }

        public String getLastName() {
            return lastName;
        }

        public int getAge() {
            return age;
        }
    }
}
```

## Aprofundando-se:
YAML foi criado em 2001 por Clark Evans como uma alternativa mais acessível e legível ao formato XML. Outra opção popular para gerenciar configurações e metadados é o JSON, mas o YAML é considerado mais expressivo e fácil de ler para humanos. A implementação do SnakeYAML é baseada na especificação YAML 1.1 e oferece suporte para objetos personalizados e conversão bidirecional entre YAML e objetos Java.

## Veja também:
- Documentação do SnakeYAML: https://bitbucket.org/asomov/snakeyaml/wiki/Documentation
- Exemplos do SnakeYAML: https://bitbucket.org/asomov/snakeyaml/wiki/Documentation#markdown-header-examples
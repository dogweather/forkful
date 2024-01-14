---
title:                "Gleam: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

##Por que usar YAML em programação?

Se você é um programador iniciante ou experiente, é provável que já tenha ouvido falar de YAML. Mas por que esse formato de linguagem de marcação é tão popular na programação? A resposta é simples: o YAML é um formato de dados fácil de ler e escrever, além de ser altamente compatível com várias linguagens de programação.

##Como usar YAML em Gleam

Para começar a trabalhar com YAML no Gleam, é necessário primeiro importar o módulo "Yaml" usando a palavra-chave `import`. Em seguida, você pode usar a função `Yaml.to_string` para converter um dado em YAML.

```
import Yaml

fn main() {
  let data = { name: "João", age: 25 }
  let yaml = Yaml.to_string(data)

  // Output: { name: "João", age: 25 }
  io.println(yaml)
}
```

Se você quiser converter um arquivo YAML para um tipo de dados Gleam, pode usar a função `Yaml.from_file`. Veja o exemplo abaixo:

```
import Yaml

fn main() {
  let data = Yaml.from_file("pessoa.yaml")
  // Output: [ { name: "Maria", age: 30 }, { name: "Lucas", age: 28 } ]
  io.println(data)
}
```

##Aprofundando-se em YAML

O YAML possui uma estrutura simples de chave-valor que é facilmente legível por humanos. É possível também usar listas e aninhar estruturas de dados YAML. Além disso, o Gleam fornece funções convenientes para trabalhar com dados YAML, como `Yaml.from_string` e `Yaml.to_file`.

Se você quiser saber mais sobre o formato YAML e suas especificações, recomenda-se consultar a documentação oficial em [yaml.org](https://yaml.org/).

##Veja também

- Documentação oficial do YAML: https://yaml.org/
- Documentação do módulo Gleam YAML: https://gleam.run/modules/yaml.html
- Exemplos de uso do YAML em Gleam: https://github.com/gleam-lang/gleam/tree/master/examples/yaml
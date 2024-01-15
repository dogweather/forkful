---
title:                "Trabalhando com yaml."
html_title:           "Swift: Trabalhando com yaml."
simple_title:         "Trabalhando com yaml."
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que trabalhar com YAML?

Se você está desenvolvendo um aplicativo iOS ou macOS, provavelmente já ouviu falar sobre YAML (YAML Ain't Markup Language). Ele é uma linguagem de marcação leve e fácil de entender, projetada para ser legível por humanos e máquinas. Com YAML, você pode armazenar dados estruturados em um formato simples e fácil de editar.

## Como usar YAML em Swift

Para começar a trabalhar com YAML em Swift, primeiro você precisa instalar uma biblioteca chamada `YamlSwift`. Você pode fazer isso facilmente usando o gerenciador de pacotes `CocoaPods`. Basta adicionar `pod 'YamlSwift'` ao seu `Podfile` e, em seguida, executar `pod install` no terminal.

Depois de instalar a biblioteca, você pode importá-la no seu arquivo Swift usando o comando `import YamlSwift`. Então, você pode começar a trabalhar com YAML utilizando a classe `Yaml` fornecida pela biblioteca.

Aqui está um exemplo simples de como criar um objeto YAML e, em seguida, convertê-lo para uma string:

```Swift
let yamlObject = Yaml.dictionary(["nome": "João", "idade": 25])
let yamlString = yamlObject.description
print(yamlString)
```

A saída seria:

```
nome: João
idade: 25
```

## Mergulho profundo em YAML

Além das operações básicas de criação e conversão de objetos YAML, você também pode usar a biblioteca `YamlSwift` para fazer consultas mais avançadas nos seus dados. Por exemplo, você pode usar a função `load()` para carregar um arquivo YAML diretamente para o seu código Swift, ou a função `parse()` para analisar uma string yaml em um objeto `Yaml`.

Além disso, a biblioteca fornece métodos para acessar e manipular os dados dentro de um objeto YAML. Por exemplo, você pode usar a função `[].string` para acessar o valor de uma chave específica em um objeto YAML, ou usar a função `removeAll()` para limpar todos os dados de um objeto.

Para mais informações sobre como trabalhar com YAML em Swift, confira a documentação oficial da biblioteca `YamlSwift`.

## Veja também

- [Documentação oficial YamlSwift](https://github.com/behrang/YamlSwift)
- [Tutorial completo de YAML para iniciantes](https://www.tutorialspoint.com/yaml/index.htm)
- [Artigo sobre o uso de YAML em aplicações iOS](https://gabrieltheodoropoulos.com/using-yaml-in-swift/)
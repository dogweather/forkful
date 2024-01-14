---
title:                "Swift: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que trabalhar com YAML?

YAML é uma linguagem de marcação de texto simples e intuitiva, que é amplamente utilizada na programação. Ela é especialmente útil para armazenar dados em formato legível por humanos, o que torna seu uso bastante versátil. Além disso, o YAML é compatível com uma grande variedade de linguagens de programação, tornando-se uma opção popular entre os desenvolvedores.

## Como usar o YAML em Swift

Para começar a trabalhar com YAML em Swift, é necessário primeiro importar a biblioteca YAML que se encontra disponível no [GitHub] (https://github.com/behrang/YamlSwift). Em seguida, você pode utilizar a função `Yaml.load()` para carregar um arquivo YAML e convertê-lo em um objeto do tipo `YamlValue`. Veja um exemplo abaixo:

```
import YamlSwift

if let path = Bundle.main.path(forResource: "arquivo", ofType: "yaml") {
    let yaml = try String(contentsOfFile: path, encoding: .utf8)

    if let obj = try Yaml.load(yaml).object {
        print(obj) // Prints the parsed YAML object
    }
} else {
    print("Arquivo não encontrado")
}
```

A partir do objeto `YamlValue`, você pode acessar os dados do arquivo YAML através de suas propriedades. Por exemplo, para acessar uma lista de itens, utilize `.array` e para acessar um valor específico, utilize `.string`.

## Aprofundando no uso de YAML

Além de carregar arquivos YAML, o Swift também oferece a possibilidade de criar e editar arquivos YAML por meio de sua sintaxe de dicionário. Isso pode ser feito através da função `Yaml.dump()`, que converte um objeto do tipo `YamlValue` em uma string YAML formatada.

Além disso, o YAML oferece suporte a funções mais avançadas, como a inclusão de referências a outros arquivos YAML e a definição de âncoras e aliases. Essas funcionalidades são especialmente úteis quando se trabalha com arquivos YAML extensos e complexos.

## Veja também

- [YAML: uma introdução] (https://forum.da2k.com.br/t/yaml-uma-introducao/)
- [Documentação oficial do YAML] (https://yaml.org/)
- [YAML vs. JSON] (https://medium.com/@veppa/yaml-vs-json-4c08a17c2ed1)
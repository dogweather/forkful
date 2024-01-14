---
title:                "C#: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que trabalhar com YAML pode ser benéfico

Você provavelmente já ouviu falar de YAML, uma linguagem de marcação utilizada principalmente para configurações de arquivos. Mas por que alguém decidiria utilizar YAML em vez de outras opções? Existem diversas razões pelas quais YAML pode ser uma escolha útil e eficiente.

Em primeiro lugar, YAML é uma linguagem fácil de ler e escrever. Seu formato é baseado em espaçamento, o que facilita a compreensão do código mesmo para iniciantes. Além disso, ele é compatível com várias linguagens de programação, incluindo C#, o que torna sua utilização versátil.

## Como trabalhar com YAML em C#

Se você está interessado em aprender como utilizar YAML em seus projetos em C#, aqui vão alguns passos simples:

### 1. Instale o pacote NuGet YAMLDotNet

O primeiro passo é adicionar o pacote NuGet YAMLDotNet ao seu projeto. Isso irá facilitar a manipulação de arquivos YAML em sua aplicação.

### 2. Criando e lendo um arquivo YAML

Agora que o pacote está instalado, vamos criar e ler um arquivo YAML. Primeiramente, vamos importar as bibliotecas necessárias:

```C#
using System.IO;
using YamlDotNet.RepresentationModel;
using YamlDotNet.Serialization;
```

Em seguida, vamos criar um arquivo YAML com algumas informações:

```C#
var conteudo = @"nome: João
sobrenome: Silva
idade: 30
profissão: engenheiro";
```

Agora, podemos ler o conteúdo do arquivo criado utilizando a classe `YamlStream`:

```C#
var input = new StringReader(conteudo);
var yaml = new YamlStream();
yaml.Load(input);
```

Por último, podemos obter os dados do arquivo YAML e exibi-los na tela:

```C#
var mapping = (YamlMappingNode)yaml.Documents[0].RootNode;
Console.WriteLine("Nome: " + mapping.Children[new YamlScalarNode("nome")]);
Console.WriteLine("Sobrenome: " + mapping.Children[new YamlScalarNode("sobrenome")]);
Console.WriteLine("Idade: " + mapping.Children[new YamlScalarNode("idade")]);
Console.WriteLine("Profissão: " + mapping.Children[new YamlScalarNode("profissão")]);
```

### 3. Manipulando dados do arquivo YAML

Além de ler dados do arquivo YAML, também podemos modificá-los. Por exemplo, podemos adicionar uma nova informação ao nosso arquivo criado anteriormente:

```C#
mapping.Children.Add("país", new YamlScalarNode("Brasil"));
```

E para salvar essas alterações, podemos utilizar o seguinte código:

```C#
var serializer = new Serializer();
using (var writer = new StringWriter())
{
    serializer.Serialize(writer, mapping);
    var novoConteudo = writer.ToString();
}
```

Isso irá gerar uma nova string com o conteúdo atualizado do arquivo YAML.

## Aprofundando no uso de YAML

Como mencionado anteriormente, YAML possui uma variedade de recursos que o tornam uma opção atraente. Por exemplo, ele suporta tipos de dados complexos, como listas e objetos, o que pode ser muito útil para configurações mais complexas.

Além disso, YAML também permite comentários em seu código, o que facilita a compreensão e organização do mesmo. Também é possível incluir blocos de texto, permitindo uma melhor segmentação do conteúdo.

Outro recurso interessante é a possibilidade de incluir referências a outros arquivos YAML, o que pode facilitar a manutenção de configurações compartilhadas entre diferentes projetos.

## Veja também

- [Documentação oficial do YAML](https://yaml.org/)
- [Tutorial de YAML em C#](https://www.c-sharpcorner.com/UploadFile/mahesh/yaml-serialization-in-C-Sharp/)
- [Exemplos práticos de utilização de YAML em projetos C#](https://codeopinion.com/does-yaml-make-sense-for-database-schema-migration/)
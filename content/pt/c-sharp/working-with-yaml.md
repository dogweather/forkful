---
title:                "Trabalhando com yaml"
html_title:           "C#: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

##Por que

Se você é um desenvolvedor de software que trabalha com aplicações ou infraestrutura baseadas na nuvem, provavelmente já se deparou com o formato YAML. Ele é amplamente usado para configurar, documentar e construir aplicações e sistemas na nuvem. Neste artigo, vamos explorar por que o YAML é uma ferramenta importante para todo desenvolvedor.

##Como Fazer

Para começar a trabalhar com YAML em C#, é essencial ter um entendimento básico da sintaxe e estrutura do YAML. YAML é um formato de dados simples e legível por humanos que pode ser facilmente criado e editado em qualquer editor de texto.

A seguir, vamos ver como podemos criar um arquivo YAML básico em C#:

````C#
using YamlDotNet.Serialization; // importar a biblioteca de serialização YAML

// definir uma classe de modelo
public class Carro
{
    public string Marca { get; set; }
    public string Modelo { get; set; }
    public int Ano { get; set; }
}

// criar um objeto carro
Carro carro = new Carro();
carro.Marca = "Fiat";
carro.Modelo = "Uno";
carro.Ano = 2010;

// usar o serializador YAML para converter o objeto em YAML
var serializer = new Serializer();
string yaml = serializer.Serialize(carro);

// imprimir o YAML no console
Console.WriteLine(yaml); 

````

Este código irá gerar o seguinte output:

```
marca: Fiat
modelo: Uno
ano: 2010
```

Agora, vamos dar uma olhada em alguns recursos mais avançados do YAML. Por exemplo, você pode usar referências para evitar repetir os mesmos dados várias vezes dentro do mesmo documento YAML. Considere o seguinte exemplo:

````C#
using YamlDotNet.Serialization;
using System.Collections.Generic;

// definir uma classe de modelo
public class Endereco
{
    public string Rua { get; set; }
    public string Cidade { get; set; }
}

public class Cliente
{
    public string Nome { get; set; }
    public Endereco Endereco { get; set; }
}

// criar objetos cliente e endereço
Endereco endereco = new Endereco();
endereco.Rua = "Rua da Paz";
endereco.Cidade = "São Paulo";

Cliente cliente1 = new Cliente();
cliente1.Nome = "João";
cliente1.Endereco = endereco;

Cliente cliente2 = new Cliente();
cliente2.Nome = "Maria";
cliente2.Endereco = endereco;

// criar uma lista de clientes
List<Cliente> clientes = new List<Cliente>();
clientes.Add(cliente1);
clientes.Add(cliente2);

// usar o serializador YAML com suporte a referências
var serializer = new SerializerBuilder().Build();
string yaml = serializer.Serialize(clientes);

// imprimir o YAML no console
Console.WriteLine(yaml); 

````

Este código irá gerar o seguinte output:

```
- nome: João
  endereco: &1
    rua: Rua da Paz
    cidade: São Paulo
- nome: Maria
  endereco: *1
```

Este é apenas um exemplo do que é possível fazer com YAML em C#. Para mais informações e exemplos, você pode consultar a documentação oficial do YamlDotNet ou explorar a biblioteca de forma mais profunda em seu projeto.

##Deep Dive

O formato YAML, abreviação de "YAML Ain't Markup Language", foi criado por Clark Evans em 2001. Ele foi desenvolvido para ser mais fácil de ler e escrever do que seus antecessores, como XML e JSON. Por ser um formato baseado em texto, o yaml pode ser facilmente versionado e compartilhado através de sistemas de controle de versão.

O YAML também possui uma estrutura flexível e extensível que permite a inclusão de metadados e comentários, tornando-o uma opção popular para a configuração de projetos de software. Além disso, ele é suportado por uma grande variedade de linguagens de programação, incluindo C#, o que o torna uma escolha versátil para desenvolvedores.

##Veja Também

Aqui estão alguns links úteis para começar a trabalhar com YAML em C#:

- Documentação oficial do YamlDotNet: https://github.com/aaubry/YamlDotNet/wiki
- Tutorial básico de YamlDotNet: https://dotnetcoretutorials.com/2020/04/01/reading-and-writing-yaml-in
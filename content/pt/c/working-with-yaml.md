---
title:                "Trabalhando com yaml"
html_title:           "C: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## O que e por que?

YAML é uma linguagem de serialização de dados, o que significa que ele permite que você salve dados em um formato legível para humanos e também para máquinas. Programadores frequentemente trabalham com YAML porque ele é fácil de escrever, ler e compartilhar, além de ser uma opção popular para armazenar dados estruturados.

## Como fazer:

Para começar a trabalhar com YAML em C, você precisa incluir o cabeçalho `yaml.h` no seu código:

```C
#include <yaml.h>
```

Em seguida, você pode começar a usar as funções disponíveis para manipular dados YAML. Por exemplo, aqui está um código simples para criar um novo documento YAML e escrever alguns dados nele:

```C
// Criar o documento
yaml_document_t documento;
yaml_document_initialize(&documento, NULL, NULL, NULL, 0, 0);

// Adicionar dados ao documento
yaml_node_t* n1 = yaml_document_add_scalar(&documento, NULL, "nome: João");
yaml_node_t* n2 = yaml_document_add_scalar(&documento, NULL, "idade: 30");

// Serializar o documento e imprimir na tela
int tamanho = yaml_document_to_str(&documento, buffer, sizeof(buffer));
printf("%s", buffer);

// Limpar memória
yaml_document_delete(&documento);
```

Isso resultará na seguinte saída:

```
nome: João
idade: 30
```

## Profundando:

YAML foi originalmente criado por Clark Evans em 2001 e desde então tem sido amplamente adotado pelos programadores. Existem também alternativas para trabalhar com dados estruturados, como JSON e XML, mas YAML é frequentemente escolhido por sua simplicidade e facilidade de uso.

Para implementar YAML em seu código C, você também pode usar a biblioteca libYAML, que fornece a funcionalidade para ler e escrever documentos YAML. Além disso, existem muitos recursos online disponíveis para ajudá-lo a aprender e aprofundar seu conhecimento sobre YAML.

## Veja também:

Para mais informações sobre YAML e como usá-lo em sua programação, confira os seguintes links:

- [Página oficial do YAML](https://yaml.org/)
- [Documentação da biblioteca libYAML](https://pyyaml.org/wiki/LibYAML)
- [Tutorial de YAML para desenvolvedores C](https://www.ibm.com/developerworks/library/l-yaml/)
- [Comparação entre JSON, XML e YAML](https://www.educba.com/json-vs-xml-vs-yaml/)
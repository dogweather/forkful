---
title:                "Trabalhando com yaml"
html_title:           "Arduino: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Trabalhar com YAML no Arduino é uma maneira de estruturar e organizar dados de forma clara e legível, usando apenas texto simples. Os programadores utilizam o YAML para armazenar configurações, dados e até mesmo código, em vez de usar o formato padrão do Arduino (C++).

## Como Fazer:

### Exemplo 1:
```Arduino
#include <yaml.h>
void setup() {
  YAML::Node dados; //cria um objeto para armazenar os dados
  dados["nome"] = "João"; //define a chave "nome" com o valor "João"
  dados["idade"] = 25; //define a chave "idade" com o valor 25
  dados["linguagem"] = "C++"; //define a chave "linguagem" com o valor "C++"

  Serial.begin(9600); //inicia a comunicação serial
  YAML::Emitter saida; //cria um objeto para gerar a saída YAML
  saida << dados; // adiciona os dados ao objeto de saída

  Serial.println(saida.c_str()); // imprime a saída do YAML na porta serial
}

void loop() {
  //não há nada a ser feito aqui
}

```
O resultado desta implementação é a impressão dos dados em formato YAML na porta serial:
```
nome: João
idade: 25
linguagem: C++
```
### Exemplo 2:
```Arduino
#include <yaml.h>
void setup() {
  YAML::Node dados; //cria um objeto para armazenar os dados
  dados["time"] = "Flamengo"; //define a chave "time" com o valor "Flamengo"
  dados["pontuação"] = 84; //define a chave "pontuação" com o valor 84
  dados["posição"] = 1; //define a chave "posição" com o valor 1

  Serial.begin(9600); //inicia a comunicação serial
  YAML::Emitter saida; //cria um objeto para gerar a saída YAML
  saida << dados; // adiciona os dados ao objeto de saída

  Serial.println(saida.c_str()); // imprime a saída do YAML na porta serial
}

void loop() {
  //não há nada a ser feito aqui
}

```

O resultado desta implementação é a impressão dos dados em formato YAML na porta serial:
```
time: Flamengo
pontuação: 84
posição: 1
```

## Mergulho Profundo:

### Contexto Histórico:
O formato YAML (YAML Ain't Markup Language), foi criado em 2001 pelo designer de software Clark Evans com o objetivo de ser uma alternativa mais simples e fácil de usar do que o formato XML. No entanto, só ganhou popularidade no meio da programação com a ascensão de novas ferramentas como o GitHub e o Kubernetes, que utilizam o formato YAML para gerenciar e configurar seus serviços.

### Alternativas:
Existem outras opções de formato de dados estruturados, como o JSON e o XML, no entanto, o YAML se destaca por ser mais legível e fácil de manipular pelos seres humanos.

### Detalhes de Implementação:
Para utilizar o YAML no Arduino, é necessário instalar a biblioteca "ArduinoYaml" disponível no gerenciador de bibliotecas da IDE. Além disso, é necessário incluir a biblioteca no seu código e utilizar as classes e funções destinadas a manipulação de dados e geração de saída no formato YAML.

## Veja Também:

- [Site oficial do YAML](https://yaml.org/)
- [Explicação detalhada sobre o formato YAML](https://medium.com/@HudsonGIslander/what-is-yaml-42a17a1ab152)
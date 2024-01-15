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

## Por que

Se você está interessado em criar programas em Arduino, é importante conhecer diversas formas de armazenar e manipular dados. O YAML é uma linguagem de marcação simples e eficiente, muito utilizada para armazenar informações em arquivos de configuração, aplicativos web e até mesmo em projetos de hardware. Aprender a trabalhar com YAML pode expandir suas habilidades em programação e torná-lo mais versátil e eficiente em sua jornada com o Arduino. 

## Como fazer

Antes de começar a escrever seu código, é necessário instalar uma biblioteca no seu Arduino IDE para permitir a manipulação de arquivos YAML. No menu "Sketch", escolha "Include Library" e em seguida "Manage Libraries". Na barra de pesquisa, digite "YAML" e escolha a biblioteca "Arduino YAML". Clique em "Install" e aguarde a instalação ser concluída.

Agora, vamos para a parte prática. Primeiramente, inclua a biblioteca no seu código usando `#include <arduino-yaml.h>`. Para ler um arquivo YAML, utilize o seguinte código:

```
ArduinoYAML yaml;

void setup() {
  Serial.begin(9600);
  // abra o arquivo em modo leitura
  File file = SD.open("dados.yaml", FILE_READ);
  // faça o parsing dos dados do arquivo
  YAML::Node doc = yaml.parse(file);
  // imprima o valor de "mensagem"
  Serial.println(doc["mensagem"].as<String>());
}

void loop() {
  // nada a ser feito no loop
}
```

Caso queira escrever em um arquivo YAML, utilize o seguinte código como base:

```
ArduinoYAML yaml;

void setup() {
  // abra o arquivo em modo escrita
  File file = SD.open("dados.yaml", FILE_WRITE);
  // crie um novo objeto YAML::Emitter
  YAML::Emitter out;
  // escreva os dados desejados, seguindo a estrutura chave-valor
  out << YAML::BeginMap;
  out << YAML::Key << "nome";
  out << YAML::Value << "João";
  out << YAML::Key << "idade";
  out << YAML::Value << 27;
  out << YAML::EndMap;
  // salve os dados no arquivo
  yaml.save(file, out);
}

void loop() {
  // nada a ser feito no loop
}
```

O arquivo "dados.yaml" será criado e conterá os seguintes dados:

```
nome: "João"
idade: 27
```

A partir desses exemplos, você pode adaptar o código à sua necessidade, criando ou lendo dados mais complexos em arquivos YAML. Lembre-se de sempre fechar o arquivo após o uso com `file.close()`.

## Mergulho profundo

O YAML (YAML Ain't Markup Language) é uma linguagem de marcação simples e legível por humanos, inspirada em outros formatos como XML e JSON. Sua estrutura é baseada em chaves e valores, organizadas em hierarquias através de identação. Por exemplo:

```
frutas:
  - banana
  - laranja
  - maçã
cores:
  - vermelho
  - verde
  - amarelo
```

Notamos que "frutas" e "cores" são chaves que contêm listas de valores. Neste caso, "banana", "laranja" e "maçã" são frutas, enquanto "vermelho", "verde" e "amarelo" são cores. Além disso, é possível adicionar comentários em um arquivo YAML usando o caractere cerquilha (#) antes do texto.

O manipulador de arquivos YAML utilizado na biblioteca para Arduino é baseado na biblioteca "ArduinoJson", o que garante compatibilidade e facilidade de uso. Outro diferencial do YAML é a possibilidade de referenciar dados em diferentes partes do arquivo usando âncoras e referências, o que pode ser útil em casos de repetição de dados.

## Veja também

- [Documentação oficial da biblioteca Arduino YAML](https://github.com/arduino-libraries/ArduinoYaml)
- [Exemplos práticos de uso de YAML com Arduino](https://create.arduino.cc/projecthub/arduino-yaml/getting-started-with-yaml-and-arduino-4af603)
- [Tutorial para iniciantes em YAML](https://www.learnpython.org/pt/YAML)
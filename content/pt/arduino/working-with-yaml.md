---
title:                "Arduino: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

##Por que trabalhar com YAML no Arduino?

O YAML é uma linguagem de marcação bem estruturada e de fácil leitura que pode ser muito útil para programadores do Arduino. Com YAML, você pode criar e enviar configurações e dados de forma simplificada e organizada. Além disso, ele é compatível com várias plataformas, o que o torna uma escolha popular para trabalhar com dispositivos eletrônicos como o Arduino.

##Como usar YAML no Arduino

Para começar a trabalhar com YAML no Arduino, você precisará incluir a biblioteca da linguagem no seu projeto. Você pode fazer isso através do Gerenciador de Bibliotecas da IDE do Arduino ou baixando o arquivo YAML diretamente da fonte.

Uma vez que a biblioteca esteja instalada, você pode começar a utilizar as funções e estruturas oferecidas por ela. Por exemplo, você pode definir uma configuração simples de WiFi usando YAML através do código abaixo:

```Arduino
#include <YAML.h>

YAML::Node config = YAML::Load("WiFi:\n  SSID: seu_nome_de_rede\n  Password: sua_senha");
```

O código acima cria uma estrutura YAML com a configuração do WiFi, que pode ser facilmente modificada e enviada para o seu dispositivo. Com YAML, você pode enviar dados mais complexos, como configurações de sensores ou atualizações de firmware, de forma organizada e fácil de ler.

##Aprofundando no YAML

Uma das grandes vantagens do YAML é a sua capacidade de serializar e desserializar dados. Isso significa que você pode facilmente converter os seus dados em uma estrutura YAML e vice-versa.

Outra característica interessante do YAML é a sua capacidade de referenciar valores em diferentes partes da estrutura. Isso pode ser útil quando se trabalha com dados que se repetem em diferentes seções do seu projeto.

Além disso, o YAML possui uma sintaxe clara e intuitiva, o que torna a criação e edição de estruturas muito mais fácil e rápida.

##Veja também

- [Documentação oficial do YAML](https://yaml.org/)
- [Biblioteca YAML para Arduino](https://github.com/jbeder/yaml-cpp)
- [Exemplo de uso de YAML no Arduino](https://www.arduino.cc/reference/en/libraries/yaml/)
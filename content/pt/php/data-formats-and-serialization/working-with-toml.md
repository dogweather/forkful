---
date: 2024-01-26 04:24:44.577346-07:00
description: "Como fazer: Primeiro, certifique-se de que voc\xEA tenha instalada uma\
  \ biblioteca parser de TOML, como a `yosymfony/toml`. Vamos analisar um arquivo\
  \ TOML."
lastmod: '2024-03-13T22:44:46.690613-06:00'
model: gpt-4-0125-preview
summary: "Primeiro, certifique-se de que voc\xEA tenha instalada uma biblioteca parser\
  \ de TOML, como a `yosymfony/toml`."
title: Trabalhando com TOML
weight: 39
---

## Como fazer:
Primeiro, certifique-se de que você tenha instalada uma biblioteca parser de TOML, como a `yosymfony/toml`. Vamos analisar um arquivo TOML:

```php
composer require yosymfony/toml

<?php
require 'vendor/autoload.php';

use Yosymfony\Toml\Toml;

$tomlString = <<<TOML
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
TOML;

$array = Toml::Parse($tomlString);

print_r($array);
```

Saída de Exemplo:

```
Array
(
    [database] => Array
        (
            [server] => 192.168.1.1
            [ports] => Array
                (
                    [0] => 8001
                    [1] => 8001
                    [2] => 8002
                )

            [connection_max] => 5000
            [enabled] => 1
        )

)
```

## Aprofundando
O TOML surgiu em 2013, criado pelo co-fundador do GitHub, Tom Preston-Werner, como uma alternativa mais amigável ao XML e JSON para arquivos de configuração. Enquanto o JSON é simples para máquinas, a estrutura do TOML facilita para os olhos humanos, sem a complexidade do YAML.

Alternativas ao TOML incluem JSON, YAML e XML. Cada um tem suas forças e cenários de aplicação. JSON é ubíquo e independente de linguagem; YAML é mais legível e suporta comentários, enquanto XML é extenso e amplamente suportado.

Ao implementar TOML em PHP, você está olhando para bibliotecas que analisam seu conteúdo em arrays ou objetos PHP. `yosymfony/toml` é um parser PHP que adere à versão v0.4.0 da especificação TOML. Para se manter atualizado, sempre verifique por parsers mais novos ou atualizações que suportem a versão mais atual do TOML (v1.0.0 até minha última atualização).

## Veja Também
- Especificação do TOML: <https://toml.io/>
- Parser de TOML para PHP (`yosymfony/toml`): <https://github.com/yosymfony/toml>
- Comparando Formatos de Dados (XML, JSON, YAML, TOML): <https://www.loginradius.com/blog/engineering/comparing-data-interchange-formats/>
- Gerenciador de Pacotes PHP (Composer): <https://getcomposer.org/>

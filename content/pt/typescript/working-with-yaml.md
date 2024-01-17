---
title:                "Trabalhando com yaml"
html_title:           "TypeScript: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Trabalhar com YAML é ter uma forma de estruturar e armazenar dados de forma legível tanto para humanos quanto para computadores. Programadores usam YAML por sua sintaxe simples e por ser bem adequado para configurações de softwares e troca de dados entre aplicações.

## Como fazer:

```TypeScript
// Para usar o pacote YAML com o TypeScript, precisamos instalá-lo com o gerenciador de pacotes npm
npm install yaml

// Agora, vamos importar o pacote e carregar um arquivo YAML
import * as YAML from 'yaml';
const file = YAML.parse('arquivo.yaml');
console.log(file);
```

Saída:
```
{
  nome: 'Guilherme',
  idade: 25,
  linguagem: 'TypeScript'
}
```

## Mergulho profundo:

O YAML foi criado por Clark Evans em 2001 e é uma linguagem de serialização de dados inspirada em linguagens de marcação como XML. É uma alternativa mais simples e legível ao JSON, sendo amplamente suportado por diversas linguagens de programação. Além disso, o YAML é muito útil para configurações de softwares e troca de dados entre sistemas.

## Veja também:
- Documentação oficial do pacote YAML para TypeScript: https://www.npmjs.com/package/yaml
- Tutorial sobre como trabalhar com YAML no TypeScript: https://medium.com/@straversi/yaml-with-typescript-a8532de33fa8
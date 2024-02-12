---
title:                "Trabalhando com YAML"
aliases:
- /pt/javascript/working-with-yaml.md
date:                  2024-02-03T19:25:36.211417-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?

YAML, abreviação de YAML Ain't Markup Language (YAML Não é Uma Linguagem de Marcação), é um formato de serialização de dados legível por humanos. Programadores frequentemente o usam para arquivos de configuração e troca de dados entre linguagens devido à sua simplicidade e legibilidade em comparação ao JSON ou XML.

## Como fazer:

Em JavaScript, trabalhar com YAML geralmente envolve o uso de uma biblioteca de terceiros, uma vez que a linguagem não inclui um analisador integrado para YAML. Uma das bibliotecas mais populares para esse propósito é `js-yaml`. Você pode usar `js-yaml` para analisar YAML em objetos JavaScript e vice-versa.

Primeiro, você precisa instalar `js-yaml`:

```bash
npm install js-yaml
```

Então, você pode usá-lo em seus projetos. Aqui está como você pode carregar um arquivo YAML e analisá-lo em um objeto JavaScript:

```javascript
// Requer o módulo js-yaml
const yaml = require('js-yaml');
const fs   = require('fs');

// Carrega YAML de um arquivo
try {
  const doc = yaml.load(fs.readFileSync('./config.yaml', 'utf8'));
  console.log(doc);
} catch (e) {
  console.error(e);
}
```

Se o seu arquivo `config.yaml` for assim:

```yaml
version: 1
services:
  web:
    image: "myapp/web:latest"
    ports:
      - "5000:5000"
```

O resultado será:

```javascript
{ version: 1,
  services: 
   { web: 
      { image: 'myapp/web:latest',
        ports: [ '5000:5000' ] } } }
```

Para fazer o inverso, convertendo um objeto JavaScript em uma string YAML:

```javascript
const yaml = require('js-yaml');
const obj = {
  version: 1,
  services: {
    web: {
      image: "myapp/web:latest",
      ports: ["5000:5000"]
    }
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

Este código produzirá:

```yaml
version: 1
services:
  web:
    image: myapp/web:latest
    ports:
      - '5000:5000'
```

Usando `js-yaml`, você pode facilmente integrar análise e serialização YAML em seus projetos JavaScript, melhorando a intercambialidade de dados e a gestão de configuração.

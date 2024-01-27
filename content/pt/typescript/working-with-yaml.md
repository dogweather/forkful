---
title:                "Trabalhando com YAML"
date:                  2024-01-19
html_title:           "Arduino: Trabalhando com YAML"
simple_title:         "Trabalhando com YAML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?
YAML é um formato de serialização de dados, fácil de ler por humanos, usado normalmente para arquivos de configuração. Programadores utilizam YAML por sua clareza e compatibilidade com várias linguagens de programação.

## Como Fazer:

Instale um módulo YAML para TypeScript, como `js-yaml`, para começar.

```bash
npm install js-yaml
```

Aqui está um exemplo de como ler e parsear um arquivo YAML em TypeScript:

```TypeScript
import fs from 'fs';
import yaml from 'js-yaml';

try {
  const configFile = fs.readFileSync('config.yaml', 'utf8');
  const config = yaml.load(configFile);
  console.log(config);
} catch (e) {
  console.error(e);
}
```

Se o seu `config.yaml` for assim:

```yaml
port: 8080
mode: 'production'
```

A saída será:

```TypeScript
{ port: 8080, mode: 'production' }
```

Para escrever um objeto em um arquivo YAML, utilize:

```TypeScript
import fs from 'fs';
import yaml from 'js-yaml';

const config = {
  port: 8080,
  mode: 'development',
};

try {
  const yamlContent = yaml.dump(config);
  fs.writeFileSync('config.yaml', yamlContent, 'utf8');
  console.log('Arquivo YAML escrito com sucesso!');
} catch (e) {
  console.error(e);
}
```

## Mergulho Profundo

YAML foi criado em 2001 e significa "YAML Ain't Markup Language", um acrônimo recursivo que destaca o foco em dados em vez de documentos. O JSON e XML são alternativas comuns, mas YAML é preferido para configurações pela sua legibilidade. Em TypeScript, a implementação de YAML geralmente é feita usando bibliotecas externas como `js-yaml`, que fazem o trabalho pesado do parsing e serialização de forma segura.

## Veja Também

- Documentação oficial de YAML: https://yaml.org/
- NPM `js-yaml`: https://www.npmjs.com/package/js-yaml
- Guia de migração de JSON para YAML: https://www.json2yaml.com/

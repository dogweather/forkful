---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:50.085046-07:00
description: "Como Fazer: Trabalhar com YAML em TypeScript geralmente envolve analisar\
  \ o conte\xFAdo YAML em objetos JavaScript e, possivelmente, converter objetos\u2026"
lastmod: '2024-03-13T22:44:46.346464-06:00'
model: gpt-4-0125-preview
summary: "Trabalhar com YAML em TypeScript geralmente envolve analisar o conte\xFA\
  do YAML em objetos JavaScript e, possivelmente, converter objetos JavaScript de\
  \ volta para YAML."
title: Trabalhando com YAML
weight: 41
---

## Como Fazer:
Trabalhar com YAML em TypeScript geralmente envolve analisar o conteúdo YAML em objetos JavaScript e, possivelmente, converter objetos JavaScript de volta para YAML. Isso requer um analisador; uma escolha popular é o `js-yaml`, uma biblioteca que pode ser facilmente integrada a projetos TypeScript.

### Instalando o js-yaml
Primeiro, adicione o `js-yaml` ao seu projeto:

```bash
npm install js-yaml
```

### Analisando YAML para Objeto JavaScript
Imagine que você tem um arquivo YAML `config.yaml` com o seguinte conteúdo:

```yaml
database:
  host: localhost
  port: 5432
  username: user
  password: pass
```

Você pode ler e analisar este arquivo para um objeto JavaScript da seguinte maneira:

```typescript
import * as fs from 'fs';
import * as yaml from 'js-yaml';

// Carregar e analisar o arquivo YAML
const fileContents = fs.readFileSync('./config.yaml', 'utf8');
const data = yaml.load(fileContents) as Record<string, any>;

console.log(data);
```

**Saída de Exemplo:**

```json
{
  "database": {
    "host": "localhost",
    "port": 5432,
    "username": "user",
    "password": "pass"
  }
}
```

### Convertendo Objeto JavaScript para YAML
Se você precisar fazer o caminho inverso e converter um objeto JavaScript em uma string YAML, você pode usar o `js-yaml` da seguinte forma:

```typescript
import * as yaml from 'js-yaml';

const obj = {
  title: "Example",
  is_published: true,
  author: {
    name: "Jane Doe",
    age: 34
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

**Saída de Exemplo:**

```yaml
title: Example
is_published: true
author:
  name: Jane Doe
  age: 34
```

Este trecho converte um objeto JavaScript em uma string YAML e a exibe. Na prática, você pode escrever isso de volta para um arquivo ou usar em outras partes da sua aplicação.

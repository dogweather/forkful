---
date: 2024-01-26 04:24:05.261402-07:00
description: "Como fazer: Para trabalhar com TOML em JavaScript, voc\xEA precisar\xE1\
  \ de um analisador como o `@iarna/toml`. Primeiro, instale-o: `npm install @iarna/toml`.\u2026"
lastmod: '2024-03-13T22:44:46.985570-06:00'
model: gpt-4-0125-preview
summary: "Para trabalhar com TOML em JavaScript, voc\xEA precisar\xE1 de um analisador\
  \ como o `@iarna/toml`."
title: Trabalhando com TOML
weight: 39
---

## Como fazer:
Para trabalhar com TOML em JavaScript, você precisará de um analisador como o `@iarna/toml`. Primeiro, instale-o: `npm install @iarna/toml`. Depois, analise uma string TOML para um objeto JavaScript ou transforme um objeto JavaScript em formato TOML.

```javascript
const toml = require('@iarna/toml');

// Analisar string TOML para objeto JS
const tomlStr = `
title = "Exemplo TOML"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
`;

const parsedData = toml.parse(tomlStr);
console.log(parsedData);

// Converter objeto JS para string TOML
const jsObject = {
  title: "Exemplo TOML",
  database: {
    server: "192.168.1.1",
    ports: [8001, 8001, 8002]
  }
};

const tomlString = toml.stringify(jsObject);
console.log(tomlString);
```

## Aprofundando
TOML foi lançado pela primeira vez em 2013 por Tom Preston-Werner, um dos fundadores do GitHub. Foi projetado para superar outros formatos, como INI, por ser mais padronizado e fácil de analisar. JSON e YAML são alternativas, mas podem ser muito complexas ou muito flexíveis. A vantagem do TOML está na configuração estática, onde um formato simples e claro é preferido. Seu design permite um mapeamento direto para uma tabela hash, com chaves e valores correspondendo a nomes de propriedades e seus valores. Para uma adoção mais ampla, você pode precisar integrar ferramentas que podem converter entre TOML e outros formatos devido ao suporte variado do ecossistema.

## Veja Também
- O repositório oficial do TOML no GitHub: https://github.com/toml-lang/toml
- Comparação entre TOML vs. YAML vs. JSON: https://gist.github.com/oconnor663/9aeb4ed56394cb013a20
- Pacote npm `@iarna/toml`: https://www.npmjs.com/package/@iarna/toml

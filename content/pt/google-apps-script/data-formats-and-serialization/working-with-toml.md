---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:13.673514-07:00
description: "Como fazer: Visto que o Google Apps Script \xE9 essencialmente JavaScript\
  \ com acesso ao pacote de aplicativos do Google, trabalhar com TOML diretamente\u2026"
lastmod: '2024-03-13T22:44:46.134673-06:00'
model: gpt-4-0125-preview
summary: "Visto que o Google Apps Script \xE9 essencialmente JavaScript com acesso\
  \ ao pacote de aplicativos do Google, trabalhar com TOML diretamente dentro do Google\
  \ Apps Script requer um pouco de engenhosidade."
title: Trabalhando com TOML
weight: 39
---

## Como fazer:
Visto que o Google Apps Script é essencialmente JavaScript com acesso ao pacote de aplicativos do Google, trabalhar com TOML diretamente dentro do Google Apps Script requer um pouco de engenhosidade. O Google Apps Script não suporta nativamente o processamento de TOML, mas você pode aproveitar bibliotecas JavaScript ou escrever um analisador simples para necessidades básicas.

Vamos analisar uma simples string de configuração TOML como exemplo:

```javascript
// String TOML
var tomlString = `
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
`;

// Uma função simples de analisador de TOML para JSON
function parseTOML(tomlStr) {
  var result = {};
  var currentSection = result;
  tomlStr.split(/\r?\n/).forEach(linha => {
    linha = linha.trim();
    if (linha.startsWith('[')) { // Nova seção
      var sectionName = linha.replace(/\[|\]/g, '');
      result[sectionName] = {};
      currentSection = result[sectionName];
    } else if (linha) {
      var keyValue = linha.split('=').map(parte => parte.trim());
      var chave = keyValue[0];
      var valor = eval(keyValue[1]); // Usar eval por simplicidade; cuidado em código de produção
      currentSection[chave] = valor;
    }
  });
  return result;
}

// Testar o analisador
var configObject = parseTOML(tomlString);
console.log(configObject);

```

A saída de exemplo do `console.log` se pareceria com um objeto JSON, facilitando o acesso às propriedades de configuração dentro do Google Apps Script:

```json
{
  "database": {
    "server": "192.168.1.1",
    "ports": [8001, 8001, 8002],
    "connection_max": 5000,
    "enabled": true
  }
}
```

## Aprofundamento
TOML foi criado por Tom Preston-Werner, um dos fundadores do GitHub, para ser mais amigável para humanos do que JSON para arquivos de configuração, mantendo a capacidade de ser analisado sem ambiguidades. Tem a intenção de ser o mais simples possível, um objetivo que se alinha bem com o ethos de muitos projetos de desenvolvimento que buscam simplicidade e legibilidade em seus códigos.

No contexto do Google Apps Script, usar TOML pode introduzir algum esforço extra, dada a falta de suporte direto e a necessidade de analisá-lo manualmente ou por meio de bibliotecas de terceiros. Para projetos menores ou aqueles não profundamente integrados ao ecossistema do Google, alternativas como JSON ou até mesmo estruturas simples de pares chave-valor em propriedades de script podem ser suficientes e mais simples de implementar. No entanto, para aplicações que priorizam arquivos de configuração amigáveis para humanos e estão já comprometidas com TOML, integrar a análise de TOML por meio de scripts personalizados adiciona uma camada útil de flexibilidade e manutenção sem se desviar dos paradigmas de configuração preferidos.

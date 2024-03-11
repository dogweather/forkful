---
date: 2024-01-26 04:27:03.274792-07:00
description: "TOML, abrevia\xE7\xE3o de Tom's Obvious, Minimal Language, \xE9 um formato\
  \ de serializa\xE7\xE3o de dados semelhante ao JSON ou YAML. Programadores o utilizam\
  \ por sua\u2026"
lastmod: '2024-03-11T00:14:20.037672-06:00'
model: gpt-4-0125-preview
summary: "TOML, abrevia\xE7\xE3o de Tom's Obvious, Minimal Language, \xE9 um formato\
  \ de serializa\xE7\xE3o de dados semelhante ao JSON ou YAML. Programadores o utilizam\
  \ por sua\u2026"
title: Trabalhando com TOML
---

{{< edit_this_page >}}

## O Que & Por Quê?
TOML, abreviação de Tom's Obvious, Minimal Language, é um formato de serialização de dados semelhante ao JSON ou YAML. Programadores o utilizam por sua legibilidade humana e mapeamento direto para tipos de dados, tornando-o uma opção ideal para arquivos de configuração e intercâmbio de dados.

## Como Fazer:
Primeiramente, você precisará de um analisador de TOML. `@iarna/toml` é uma escolha popular. Instale-o com npm: `npm install @iarna/toml --save`. Veja como ler um arquivo TOML e analisá-lo para um objeto JavaScript:

```typescript
import * as fs from 'fs';
import toml from '@iarna/toml';

const conteudoToml = fs.readFileSync('config.toml', 'utf-8');
const dadosAnalisados = toml.parse(conteudoToml);

console.log(dadosAnalisados);
```
Se `config.toml` contém:
```
[server]
port = 8080
```
A saída seria:
```
{ server: { port: 8080 } }
```
E, escrever em um arquivo TOML é igualmente simples:
```typescript
import * as fs from 'fs';
import { stringify } from '@iarna/toml';

const obj = { server: { port: 8080 } };
const stringToml = stringify(obj);
fs.writeFileSync('config.toml', stringToml);
``` 
Executar este código escreve o objeto em `config.toml` no formato TOML.

## Aprofundamento
TOML foi criado por Tom Preston-Werner, co-fundador do GitHub, por volta de 2013 como uma resposta às limitações que percebeu em outros formatos como INI ou YAML. É projetado para ser inequívoco e fácil de analisar em estruturas de dados, portanto, um favorito para arquivos de configuração. Alternativas como JSON não possuem comentários, enquanto YAML é mais complexo. TOML se destaca por sua simplicidade e sua habilidade em representar claramente hierarquias de dados complexas.

Por baixo dos panos, quando você analisa TOML em TypeScript, está convertendo dados textuais em um formato estruturado que a linguagem pode manipular. Isso envolve lexing (transformar texto bruto em tokens) e parsing (construir uma estrutura de dados interna); `@iarna/toml` lida com ambos de forma transparente. O suporte a emojis é um toque divertido, mostrando a abordagem centrada no usuário do TOML.

## Veja Também
- Especificação Oficial do TOML: https://toml.io/en/
- pacote `@iarna/toml`: https://www.npmjs.com/package/@iarna/toml
- Comparativos entre TOML, YAML e JSON: https://blog.bitsrc.io/choosing-the-right-configuration-file-format-toml-vs-yaml-vs-json-71b5be8968ea

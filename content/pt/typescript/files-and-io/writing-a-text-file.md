---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:31.282845-07:00
description: "Escrever um arquivo de texto em TypeScript \xE9 uma habilidade crucial\
  \ para a persist\xEAncia de dados, configura\xE7\xF5es ou gera\xE7\xE3o de logs.\
  \ Programadores\u2026"
lastmod: '2024-02-25T18:49:43.965863-07:00'
model: gpt-4-0125-preview
summary: "Escrever um arquivo de texto em TypeScript \xE9 uma habilidade crucial para\
  \ a persist\xEAncia de dados, configura\xE7\xF5es ou gera\xE7\xE3o de logs. Programadores\u2026"
title: Escrevendo um arquivo de texto
---

{{< edit_this_page >}}

## O Que & Porquê?
Escrever um arquivo de texto em TypeScript é uma habilidade crucial para a persistência de dados, configurações ou geração de logs. Programadores frequentemente realizam essa tarefa para armazenar e manipular dados fora da memória da aplicação por razões como análise de dados, reportagem ou simplesmente para salvar as configurações do usuário entre sessões.

## Como fazer:
O TypeScript por si só não lida diretamente com operações de arquivo, pois é compilado para JavaScript, que tradicionalmente é executado no navegador com acesso limitado ao sistema de arquivos. No entanto, quando utilizado em um ambiente Node.js, o módulo `fs` (Sistema de Arquivos) fornece funcionalidades para escrever arquivos.

### Usando o módulo fs do Node.js
Primeiro, garanta que você está trabalhando em um ambiente Node.js. Então, use o módulo `fs` para escrever arquivos de texto. Aqui está um exemplo básico:

```typescript
import * as fs from 'fs';

const data = 'Olá, mundo!';
const filePath = './mensagem.txt';

fs.writeFile(filePath, data, 'utf8', (err) => {
    if (err) throw err;
    console.log('O arquivo foi salvo!');
});
```

Isso escreverá de forma assíncrona "Olá, mundo!" em `mensagem.txt`. Se o arquivo não existir, o Node.js o cria; se existir, o Node.js o sobrescreve.

Para escrita de arquivo síncrona, use `writeFileSync`:

```typescript
import * as fs from 'fs';

const data = 'Olá de novo, mundo!';
const filePath = './mensagem.txt';

try {
    fs.writeFileSync(filePath, data, 'utf8');
    console.log('O arquivo foi salvo!');
} catch (err) {
    console.error(err);
}
```

### Usando bibliotecas de terceiros populares
Embora o módulo `fs` nativo seja poderoso, alguns desenvolvedores preferem usar bibliotecas de terceiros para conveniência e funcionalidade adicionais. `fs-extra` é uma escolha popular que estende o `fs` e torna as operações de arquivo mais diretas.

Primeiro, você precisará instalar o `fs-extra`:

```
npm install fs-extra
```

Depois, você pode usá-lo em seu arquivo TypeScript para escrever conteúdo de texto:

```typescript
import * as fs from 'fs-extra';

const data = 'Isso é fs-extra!';
const filePath = './extraMessage.txt';

// Usando async/await
async function writeFile() {
    try {
        await fs.writeFile(filePath, data, 'utf8');
        console.log('O arquivo foi salvo com fs-extra!');
    } catch (err) {
        console.error(err);
    }
}

writeFile();
```

Este trecho de código faz a mesma coisa que os exemplos anteriores de `fs`, mas utiliza a biblioteca `fs-extra`, oferecendo uma sintaxe mais limpa para o tratamento de promessas.

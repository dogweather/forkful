---
title:                "Verificando se um diretório existe"
aliases: - /pt/javascript/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:43.569620-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verificando se um diretório existe"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O que & Por quê?
Verificar se um diretório existe em JavaScript é essencial para tarefas de manipulação de arquivos, permitindo que scripts verifiquem a presença do diretório antes de ler ou escrever nele. Esta operação previne erros e assegura uma execução mais suave do programa, particularmente em aplicações que manipulam arquivos ou diretórios dinamicamente baseados em entrada do usuário ou fontes de dados externas.

## Como fazer:
No Node.js, como o próprio JavaScript não tem acesso direto ao sistema de arquivos, o módulo `fs` é tipicamente usado para essas operações. Aqui está uma maneira simples de verificar se um diretório existe usando `fs.existsSync()`:

```javascript
const fs = require('fs');

const directoryPath = './sample-directory';

// Verificar se o diretório existe
if (fs.existsSync(directoryPath)) {
  console.log('O diretório existe.');
} else {
  console.log('O diretório não existe.');
}
```
**Saída de Exemplo:**
```
O diretório existe.
```
Ou, para uma abordagem assíncrona não bloqueante, use `fs.promises` com `async/await`:

```javascript
const fs = require('fs').promises;

async function checkDirectory(directoryPath) {
  try {
    await fs.access(directoryPath);
    console.log('O diretório existe.');
  } catch (error) {
    console.log('O diretório não existe.');
  }
}

checkDirectory('./sample-directory');
```
**Saída de Exemplo:**
```
O diretório existe.
```

Para projetos que fazem uso intensivo de operações de arquivo e diretório, o pacote `fs-extra`, uma extensão do módulo nativo `fs`, oferece métodos adicionais convenientes. Aqui está como você pode alcançar o mesmo com `fs-extra`:

```javascript
const fs = require('fs-extra');

const directoryPath = './sample-directory';

// Verificar se o diretório existe
fs.pathExists(directoryPath)
  .then(existe => console.log(existe ? 'O diretório existe.' : 'O diretório não existe.'))
  .catch(err => console.error(err));
```
**Saída de Exemplo:**
```
O diretório existe.
```

Esta abordagem permite um código limpo, legível que integra-se perfeitamente com as práticas modernas do JavaScript.

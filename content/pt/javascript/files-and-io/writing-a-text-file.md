---
title:                "Escrevendo um arquivo de texto"
aliases:
- /pt/javascript/writing-a-text-file.md
date:                  2024-02-03T19:28:18.931963-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escrevendo um arquivo de texto"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?
Escrever um arquivo de texto em JavaScript geralmente está relacionado a criar e salvar dados em um formato simples e legível para fins de registro (logging), exportação de entrada de usuário ou configurações. Essa funcionalidade é crucial para aplicações que precisam persistir dados além do tempo de vida do processo da aplicação, fornecendo uma maneira de armazenar e, posteriormente, recuperar ou compartilhar informações.

## Como Fazer:
Em um ambiente Node.js, você pode usar o módulo integrado `fs` (File System) para escrever arquivos de texto. Este exemplo demonstra como escrever texto em um arquivo de forma assíncrona:

```javascript
const fs = require('fs');

const data = 'Olá, Mundo! Este é o texto a ser escrito em um arquivo.';

fs.writeFile('exemplo.txt', data, (err) => {
  if (err) {
    throw err;
  }
  console.log('Arquivo foi escrito.');
});
```

Saída de exemplo:
```
Arquivo foi escrito.
```

Para escrita de arquivo síncrona, use `writeFileSync`:
```javascript
try {
  fs.writeFileSync('exemplo.txt', data);
  console.log('Arquivo foi escrito.');
} catch (error) {
  console.error('Erro ao escrever arquivo:', error);
}
```

Em navegadores web modernos, a API de Acesso ao Sistema de Arquivos introduz a capacidade de ler e escrever arquivos. No entanto, seu uso está sujeito a permissões do usuário. Veja como criar e escrever em um arquivo:

```javascript
if ('showSaveFilePicker' in window) {
  const handle = await window.showSaveFilePicker();
  const writable = await handle.createWritable();
  await writable.write('Olá, Mundo! Isso é uma escrita de arquivo de texto do navegador.');
  await writable.close();
}
```

Para cenários mais complexos ou quando trabalhando com arquivos grandes, você pode optar por bibliotecas de terceiros como `FileSaver.js` para navegadores:

```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.2/FileSaver.min.js"></script>
<script>
  const blob = new Blob(["Olá, Mundo! Este é o texto do FileSaver.js."], {type: "text/plain;charset=utf-8"});
  saveAs(blob, "exemplo.txt");
</script>
```

Lembre-se, escrever arquivos do lado do cliente (em navegadores) é restrito devido a preocupações de segurança, e qualquer operação que requer salvar no disco local do usuário geralmente requererá sua permissão explícita.

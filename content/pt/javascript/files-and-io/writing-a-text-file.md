---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:18.931963-07:00
description: "Escrever um arquivo de texto em JavaScript geralmente est\xE1 relacionado\
  \ a criar e salvar dados em um formato simples e leg\xEDvel para fins de registro\u2026"
lastmod: '2024-03-13T22:44:46.980495-06:00'
model: gpt-4-0125-preview
summary: "Escrever um arquivo de texto em JavaScript geralmente est\xE1 relacionado\
  \ a criar e salvar dados em um formato simples e leg\xEDvel para fins de registro\
  \ (logging), exporta\xE7\xE3o de entrada de usu\xE1rio ou configura\xE7\xF5es."
title: Escrevendo um arquivo de texto
weight: 24
---

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

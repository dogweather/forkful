---
title:                "Criando um arquivo temporário"
html_title:           "TypeScript: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que

Criar um arquivo temporário é uma ação comum em muitos projetos de desenvolvimento de software. É útil para armazenar dados temporários ou para testar novas funcionalidades sem afetar os arquivos originais.

## Como fazer

Para criar um arquivo temporário em TypeScript, você precisará importar o módulo "fs" e utilizar a função "writeFileSync". Veja um exemplo abaixo:

```TypeScript
import fs from 'fs';

const tempFile = fs.writeFileSync('temp.txt', 'Este é um arquivo temporário.');
```

Este código criará um arquivo chamado "temp.txt" e escreverá o texto "Este é um arquivo temporário" dentro dele.

## Mergulho profundo

Além da função "writeFileSync", o módulo "fs" também possui outras opções para trabalhar com arquivos temporários. Por exemplo, você pode utilizar a função "mkdtempSync" para criar um diretório temporário ou a função "unlinkSync" para deletar um arquivo temporário. Também é possível definir algumas opções, como o local onde o arquivo será criado ou o tipo de codificação utilizada.

Veja a documentação completa do módulo "fs" para mais detalhes e opções de uso.

## Veja também

- [Documentação do módulo "fs"](https://nodejs.org/api/fs.html)
- [Tutorial sobre como criar arquivos temporários em TypeScript](https://www.geeksforgeeks.org/how-to-create-temporary-file-in-typescript/)
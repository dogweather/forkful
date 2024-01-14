---
title:    "TypeScript: Criando um arquivo temporário"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em TypeScript?

Criar arquivos temporários é uma prática comum na programação em TypeScript. Isso pode ser útil quando estamos trabalhando com operações que requerem o uso de arquivos temporários, como a leitura e escrita de dados, depuração de código ou até mesmo para testar algumas funcionalidades. Neste artigo, vamos aprender como criar um arquivo temporário em TypeScript e explorar sua importância.

## Como criar um arquivo temporário em TypeScript?

Para criar um arquivo temporário em TypeScript, primeiro precisamos importar o módulo "fs" (filesystem). Em seguida, podemos usar a função "mkdtempSync()" para criar o arquivo temporário. Veja um exemplo abaixo: 

```TypeScript 
import * as fs from 'fs';

const nomeArquivo = fs.mkdtempSync('arquivoTemporario');
console.log(nomeArquivo); 
``` 

No exemplo acima, usamos a função "mkdtempSync()" para criar um arquivo temporário com o nome "arquivoTemporario". Em seguida, imprimimos o nome do arquivo temporário no console. Ao rodar o código, vamos obter um resultado como este: 

```
C:\Users\Usuario\AppData\Local\Temp\arquivoTemporario.12345 
``` 

## Deep Dive - Explorando mais sobre a criação de arquivos temporários em TypeScript 

Ao criar um arquivo temporário, é importante lembrar que ele será removido automaticamente quando o programa terminar de executar. Isso é importante para evitar acúmulo de arquivos temporários desnecessários e otimizar o desempenho do programa. 

Além disso, é possível passar um segundo parâmetro opcional para a função "mkdtempSync()" para definir a pasta onde o arquivo temporário será criado. Isso pode ser útil quando precisamos salvar o arquivo temporário em uma pasta específica. 

## Veja também

- [Documentação oficial do TypeScript](https://www.typescriptlang.org)
- [Guia completo de criação de arquivos em TypeScript](https://www.digitalocean.com/community/tutorials/how-to-manage-files-with-node-js-and-typescript)
- [Classe para criação de arquivos temporários em TypeScript](https://www.npmjs.com/package/tmp)
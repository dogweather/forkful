---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:26.135661-07:00
description: "Escrever um arquivo de texto no Google Apps Script permite que os desenvolvedores\
  \ armazenem dados de maneira persistente, tornando-os acess\xEDveis para uso\u2026"
lastmod: '2024-03-13T22:44:46.129061-06:00'
model: gpt-4-0125-preview
summary: "Escrever um arquivo de texto no Google Apps Script permite que os desenvolvedores\
  \ armazenem dados de maneira persistente, tornando-os acess\xEDveis para uso futuro\
  \ ou an\xE1lise."
title: Escrevendo um arquivo de texto
weight: 24
---

## Como fazer:
Criar e escrever em um arquivo de texto no Google Apps Script pode ser realizado através do serviço Google DriveApp. Abaixo está um guia passo a passo com exemplos de código para começar:

**Passo 1: Criar um Novo Arquivo de Texto**

```javascript
// Cria um novo arquivo de texto na raiz do Google Drive
var file = DriveApp.createFile('Exemplo.txt', 'Olá, mundo!');
```

Este trecho de código cria um arquivo de texto chamado "Exemplo.txt" com o conteúdo "Olá, mundo!".

**Passo 2: Abrindo e Escrevendo em um Arquivo de Texto Existente**

Se você precisar abrir um arquivo existente e escrever nele, pode usar o método `getFileById(id)` para recuperar o arquivo e depois manipular seu conteúdo.

```javascript
// Obtém um arquivo pelo seu ID e acrescenta novo conteúdo
var fileId = 'SEU_ID_DE_ARQUIVO_AQUI'; // Substitua SEU_ID_DE_ARQUIVO_AQUI pelo seu real ID de arquivo
var file = DriveApp.getFileById(fileId);
file.setContent(file.getBlob().getDataAsString() + '\nNovo conteúdo adicionado.');
```

Este código recupera um arquivo existente usando seu ID único, e então acrescenta "Novo conteúdo adicionado." ao conteúdo que estava lá anteriormente.

**Saída de Amostra**

Não é exibida uma saída explícita ao executar os trechos de código acima, mas se você navegar até o Google Drive onde o arquivo está localizado, você verá "Exemplo.txt" para o primeiro trecho de código. Para o segundo trecho, se você abrir o arquivo especificado pelo ID, deverá ver o conteúdo original seguido pela nova linha "Novo conteúdo adicionado."

## Aprofundamento
Escrever um arquivo de texto no Google Apps Script aproveita o serviço DriveApp, essencialmente aproveitando as capacidades do Google Drive para armazenamento e gerenciamento de arquivos. Esta abordagem remonta à concepção do Google Apps Script, que foi projetado para automatizar facilmente tarefas através da suíte de ferramentas de produtividade do Google, incluindo o Drive.

Enquanto manipular arquivos diretamente através do Google Apps Script é direto e integrado ao Google Workspace, desenvolvedores de outros backgrounds (por exemplo, Python, Node.js) podem achar diferente de trabalhar com um sistema de arquivos local ou outros serviços de armazenamento em nuvem como o AWS S3. Essas plataformas frequentemente oferecem um conjunto mais complexo de capacidades de manipulação de arquivos, mas exigem configuração adicional para autenticação e permissões.

Para cenários que exigem capacidades mais avançadas de gerenciamento ou processamento de arquivos além de simples arquivos de texto (como manipulação de dados binários ou operações extensivas de sistema de arquivos), desenvolvedores podem considerar o uso de serviços do Google Cloud Platform (por exemplo, Cloud Storage) em conjunto com o Google Apps Script. Tais alternativas, embora mais poderosas, também introduzem uma curva de aprendizado mais íngreme e potencialmente custos mais altos, dependendo do escopo do projeto.

Em conclusão, enquanto o Google Apps Script fornece uma maneira acessível e eficiente de gerenciar arquivos dentro do Google Drive, incluindo a escrita de arquivos de texto, é importante entender suas limitações e explorar outras tecnologias do Google conforme necessário para atender a requisitos mais complexos.

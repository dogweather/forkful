---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:51:46.843881-07:00
description: "Criar um arquivo tempor\xE1rio em Google Apps Script envolve gerar um\
  \ arquivo destinado ao uso de curto prazo, tipicamente para processamento intermedi\xE1\
  rio\u2026"
lastmod: '2024-03-13T22:44:46.130125-06:00'
model: gpt-4-0125-preview
summary: "Criar um arquivo tempor\xE1rio em Google Apps Script envolve gerar um arquivo\
  \ destinado ao uso de curto prazo, tipicamente para processamento intermedi\xE1\
  rio\u2026"
title: "Criando um arquivo tempor\xE1rio"
weight: 21
---

## O Que & Por Que?

Criar um arquivo temporário em Google Apps Script envolve gerar um arquivo destinado ao uso de curto prazo, tipicamente para processamento intermediário de dados, depuração ou propósitos de cache. Programadores fazem isso para gerenciar dados temporariamente sem sobrecarregar o espaço de armazenamento permanente ou quando a permanência dos dados é desnecessária para além do escopo do processo atual.

## Como fazer:

No Google Apps Script, criar um arquivo temporário pode ser alcançado usando o serviço DriveApp, que fornece um método direto para criar, ler e deletar arquivos no Google Drive. Aqui está como você pode criar um arquivo de texto temporário, escrever alguns dados nele e então remover após o uso:

```javascript
function createTemporaryFile() {
  // Cria um arquivo temporário chamado "tempFile.txt"
  var tempFile = DriveApp.createFile('tempFile.txt', 'Conteúdo Temporário', MimeType.PLAIN_TEXT);
  
  // Registra a URL do arquivo para acesso ou depuração
  Logger.log('Arquivo temporário criado: ' + tempFile.getUrl());
  
  // Operação de exemplo: Lendo o conteúdo do arquivo
  var content = tempFile.getBlob().getDataAsString();
  Logger.log('Conteúdo do tempFile: ' + content);
  
  // Assumindo que a operação está completa e o arquivo não é mais necessário
  // Remove o arquivo temporário
  tempFile.setTrashed(true);
  
  // Confirma a exclusão
  Logger.log('Arquivo temporário deletado');
}
```

Executar esse script resultaria em:

```
Arquivo temporário criado: [URL do arquivo temporário criado]
Conteúdo do tempFile: Conteúdo Temporário
Arquivo temporário deletado
```

Este exemplo de script mostra a criação de um arquivo temporário, realizando uma operação para ler seu conteúdo e, finalmente, removendo o arquivo para limpeza.

## Aprofundamento

O conceito de criação de arquivos temporários no desenvolvimento de software é tão antigo quanto o próprio conceito de gerenciamento de arquivos. Em sistemas de arquivos tradicionais, arquivos temporários são frequentemente criados em diretórios temp designados e são cruciais para vários processos intermediários, como ordenar grandes conjuntos de dados, segurar dados de sessão para aplicações web ou armazenar pedaços de dados durante processos de conversão de arquivos.

No Google Apps Script, o processo de criação de arquivos temporários aproveita a infraestrutura do Google Drive, o que oferece uma interessante combinação de gestão de arquivos baseada na nuvem com conceitos de programação tradicionais. No entanto, este método de criar arquivos temporários no Google Drive não é isento de suas limitações e custos, considerando os limites de cota que o Google Drive impõe. Além disso, a latência no acesso ao Google Drive através da rede em comparação a um sistema de arquivos local pode ser um fator crítico para aplicações de alto desempenho.

Como alternativas, desenvolvedores podem considerar o uso do Google Sheets para pequenos conjuntos de dados que requerem armazenamento temporário durante o cálculo, ou o Google Cloud Storage para aplicações que exigem operações de leitura/gravação de alto desempenho e maiores capacidades de armazenamento. Cada uma dessas soluções oferece diferentes compensações em relação à latência, limites de armazenamento e facilidade de uso a partir do Google Apps Script. Em última análise, a escolha depende dos requisitos específicos da aplicação e da infraestrutura existente na qual opera.

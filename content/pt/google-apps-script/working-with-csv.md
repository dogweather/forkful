---
title:                "Trabalhando com CSV"
aliases:
- pt/google-apps-script/working-with-csv.md
date:                  2024-02-01T22:05:22.348840-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/google-apps-script/working-with-csv.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O que & Por quê?

Trabalhar com arquivos CSV (Valores Separados por Vírgulas) no Google Apps Script envolve ler, modificar e escrever arquivos de texto simples onde cada linha representa um registro de dados com valores separados por vírgulas. Os programadores fazem isso para facilitar a troca de dados entre diferentes aplicações, bancos de dados ou linguagens de programação devido à ampla adoção do CSV como um formato de intercâmbio de dados baseado em texto simples.

## Como fazer:

### Lendo Dados CSV

Para ler dados CSV de um arquivo armazenado no Google Drive, você precisa primeiro obter o conteúdo do arquivo como uma string, depois analisá-lo. O Google Apps Script torna a obtenção do conteúdo do arquivo direta com o serviço DriveApp.

```javascript
function readCSV() {
  var fileId = 'YOUR_FILE_ID_HERE'; // Substitua pelo ID real do arquivo
  var file = DriveApp.getFileById(fileId);
  var content = file.getBlob().getDataAsString();
  var rows = content.split("\n");
  
  for (var i = 0; i < rows.length; i++) {
    var cells = rows[i].split(",");
    Logger.log(cells); // Registra as células de cada linha
  }
}
```

### Escrevendo Dados CSV

Criar e escrever em um CSV envolve a construção de uma string com valores separados por vírgulas e quebras de linha, depois salvando ou exportando-a. Este exemplo demonstra a criação de um novo arquivo CSV no Google Drive.

```javascript
function writeCSV() {
  var folderId = 'YOUR_FOLDER_ID_HERE'; // Substitua pelo ID da pasta do Drive onde o novo arquivo será criado
  var csvContent = "Nome,Idade,Ocupação\nJohn Doe,29,Engenheiro\nJane Smith,34,Designer";
  var fileName = "example.csv";
  
  var folder = DriveApp.getFolderById(folderId);
  folder.createFile(fileName, csvContent, MimeType.PLAIN_TEXT);
}
```

### Saída de Exemplo

Ao registrar células de linhas ao ler um CSV:

```plaintext
[John, 29, Engenheiro]
[Jane, 34, Designer]
```

Ao escrever, um arquivo chamado "example.csv" é criado com o conteúdo:

```plaintext
Nome,Idade,Ocupação
John Doe,29,Engenheiro
Jane Smith,34,Designer
```

## Aprofundamento

Historicamente, os arquivos CSV têm sido favorecidos por sua simplicidade e legibilidade humana, tornando-os acessíveis a não-programadores e úteis para tarefas rápidas de inspeção de dados. No entanto, o Google Apps Script opera dentro do reino do ecossistema do Google, onde o Google Sheets atua como uma alternativa poderosa e amigável para a manipulação de CSV. As planilhas não apenas fornecem uma GUI para edição de dados, mas também suportam fórmulas complexas, estilização e muitos mais recursos que os CSVs brutos não possuem.

Apesar das vantagens oferecidas pelo Google Sheets, a manipulação direta de CSV no Google Apps Script permanece importante para tarefas automatizadas, especialmente ao lidar com sistemas externos que geram ou requerem dados no formato CSV. Por exemplo, integrando com sistemas legados, exportando dados para uso em outras aplicações ou pré-processando antes de alimentar dados no Google Sheets.

Além disso, a capacidade do Google Apps Script de trabalhar com arquivos CSV pode ser estendida com o serviço Utilities para necessidades avançadas de codificação, ou interagir com APIs externas para tarefas de conversão, análise ou validação. No entanto, para trabalhar com grandes conjuntos de dados ou exigir manipulações complexas, considere utilizar APIs do Google Sheets ou explorar o BigQuery para capacidades de processamento de dados mais robustas.

Embora a simplicidade permaneça como uma razão chave para a popularidade do CSV, essas alternativas oferecem um conjunto mais rico de recursos para lidar com dados no amplo ecossistema do Google Cloud.

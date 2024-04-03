---
date: 2024-01-26 04:34:19.691946-07:00
description: 'Como fazer: .'
lastmod: '2024-03-13T22:44:46.821409-06:00'
model: gpt-4-0125-preview
summary: .
title: Trabalhando com XML
weight: 40
---

## Como fazer:
```PowerShell
# Carregando um arquivo XML em uma variável
[xml]$xmlContent = Get-Content 'caminho\para\seu\arquivo.xml'

# Acessando nós XML 
$books = $xmlContent.catalog.book
foreach ($book in $books) {
  Write-Output "Título: $($book.title)"
}

# Criando um novo elemento XML
$newBook = $xmlContent.CreateElement("book")
$newBook.SetAttribute("id", "bk999")
$xmlContent.DocumentElement.AppendChild($newBook)

# Salvando o XML de volta para o arquivo
$xmlContent.Save('caminho\para\seu\arquivo\atualizado.xml')
```
Saída de exemplo:
```
Título: Programando PowerShell
Título: Essenciais XML
```

## Aprofundando
O XML, ou Linguagem de Marcação Extensível, existe desde o final dos anos 90 e continua sendo um formato amplamente usado para dados estruturados. O PowerShell simplifica o trabalho com XML em comparação com métodos tradicionais de análise; ele converte o XML em objetos diretamente, permitindo interagir com os elementos através da notação por ponto familiar.

Alternativas ao XML incluem JSON, YAML ou formatos de dados personalizados. O JSON, por exemplo, ganhou popularidade por sua natureza leve e facilidade de uso com tecnologias web. No entanto, os recursos estendidos do XML como namespaces, esquemas e processamento XSLT, muitas vezes o tornam mais adequado para documentos complexos ou padrões da indústria.

O PowerShell utiliza as capacidades XML do .NET Framework para seu manuseio de XML. Isso significa que não se trata apenas de operações simples de leitura e escrita; você também pode trabalhar com esquemas XML para validação, usar XPath para consultas e empregar transformações XSLT, tudo por meio do PowerShell.

## Veja Também
- [Tutorial de XML do W3Schools](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/json-pt.html)

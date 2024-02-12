---
title:                "Trabalhando com XML"
aliases:
- /pt/powershell/working-with-xml/
date:                  2024-01-26T04:34:19.691946-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/working-with-xml.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Trabalhar com XML envolve manipular e acessar dados estruturados na Linguagem de Marcação Extensível (eXtensible Markup Language). Os programadores trabalham com XML para permitir a interoperabilidade com outros sistemas ou para ler e escrever arquivos de configuração, feeds de dados e outros documentos estruturados comuns em serviços web.

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

---
title:                "Buscando e substituindo texto"
html_title:           "PowerShell: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Substituir textos durante a programação pode ser uma tarefa tediosa e demorada. Felizmente, com o PowerShell, é possível automatizar essa atividade e agilizar o processo de desenvolvimento. 

Os programadores geralmente substituem textos para corrigir erros, padronizar códigos ou fazer mudanças em massa em seus scripts. O uso correto dessa ferramenta pode economizar tempo e aumentar a produtividade no trabalho.

## Como Fazer:

Para realizar a substituição de textos no PowerShell, utilizamos o cmdlet ```-replace```acompanhado por dois parâmetros: o texto a ser substituído e o novo texto que o substituirá.

```
PowerShell -replace “texto antigo”, “novo texto”
```

Podemos usar o operador '|' para combinar vários comandos e realizar múltiplas substituições em uma única linha:

```
(Get-Content arquivo.txt) | ForEach-Object {
    $_ -replace “texto antigo”, “novo texto” -replace “outro texto antigo”, “outro novo texto”
} | Set-Content arquivo.txt 
```

Caso queiramos fazer a substituição em todos os arquivos de um diretório, utilizamos o cmdlet ```-Path``` para especificar o caminho e utilizamos o ```-Include``` para escolher o tipo de arquivo que será afetado:

```
PowerShell -replace “texto antigo”, “novo texto” -Path C:\diretorio -Include *.txt
```

## Deep Dive:

A substituição de textos tem sido uma ferramenta essencial para programadores desde os primeiros dias da programação. No entanto, antes do PowerShell, essa tarefa era realizada através de comandos mais complexos e menos intuitivos. 

Existem alternativas para a substituição de textos, como o famoso comando ```sed``` presente em sistemas Unix. No entanto, o PowerShell oferece uma experiência mais amigável e integrada com o sistema Windows.

Para implementar a substituição de textos, o PowerShell utiliza a classe .NET ```Regex``` para encontrar correspondências e realizar as substituições. Isso também permite o uso de expressões regulares, tornando o processo mais flexível e poderoso.

## Veja Também:

Para mais informações sobre a substituição de textos no PowerShell, consulte a documentação oficial da Microsoft: [https://docs.microsoft.com/pt-br/powershell/module/microsoft.powershell.core/about/about_replace?view=powershell-7.1](https://docs.microsoft.com/pt-br/powershell/module/microsoft.powershell.core/about/about_replace?view=powershell-7.1)
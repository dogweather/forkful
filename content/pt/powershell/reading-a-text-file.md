---
title:                "Lendo um arquivo de texto"
html_title:           "Bash: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O quê e Por quê?

Ler um arquivo de texto é o processo de acessar e interpretar dados contidos em um arquivo de texto em um computador. Os programadores fazem isso para manipular esses dados, seja para análise, processamento ou alterações.

## Como fazer:

Para ler um arquivo texto utilizando o PowerShell, podemos utilizar o cmdlet `Get-Content`.

```PowerShell
# Lendo um arquivo de texto
Get-Content -Path C:\CaminhoParaOArquivo\meuArquivo.txt
```

A saída será o conteúdo do arquivo `meuArquivo.txt`. Para ler somente as primeiras 5 linhas, você pode usar o seguinte:

```PowerShell
# Lendo as primeiras 5 linhas de um arquivo
Get-Content -Path C:\CaminhoParaOArquivo\meuArquivo.txt -First 5
```

## Mergulho Profundo

O `Get-Content` é uma função muito poderosa do PowerShell, mas não é a única forma de lidar com arquivos de texto. Há também o `System.IO.File` do .NET Framework, que pode ser utilizado no PowerShell. Aqui está um exemplo:

```PowerShell
# Utilizando o .NET Framework para ler um arquivo
[System.IO.File]::ReadAllText('C:\CaminhoParaOArquivo\meuArquivo.txt')
```

Porém, um ponto importante a se notar é que, enquanto `Get-Content` lê o arquivo linha por linha (útil para arquivos grandes), `System.IO.File` lê todo o arquivo de uma vez (gerando um possível problema com arquivos grandes). 

O `Get-Content` foi introduzido na primeira versão do PowerShell como uma forma de manipular arquivos de texto sem ter que recorrer ao .NET Framework.

## Veja Também

- Guia oficial da Microsoft sobre o `Get-Content`: https://docs.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Management/Get-Content?view=powershell-7.1
- StackOverflow com perguntas relacionadas a manipulação de arquivos de texto com PowerShell: https://stackoverflow.com/questions/tagged/powershell+file-io
- Descrição mais detalhada do `System.IO.File`: https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=netframework-4.8
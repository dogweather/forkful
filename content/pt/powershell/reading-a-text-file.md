---
title:                "Lendo um arquivo de texto."
html_title:           "PowerShell: Lendo um arquivo de texto."
simple_title:         "Lendo um arquivo de texto."
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O que e por que?
Ler um arquivo de texto é uma operação comum para programadores, pois permite a leitura de informações salvas em um arquivo de texto de forma eficiente, sem a necessidade de abrir manualmente o arquivo e procurar pelos dados desejados. 

## Como fazer:
O PowerShell possui um cmdlet (comando especial) específico para ler arquivos de texto: Get-Content. Basta utilizar o caminho do arquivo como argumento e a informação será exibida no console. Veja o exemplo abaixo:

```PowerShell
Get-Content C:\Users\Usuario\Documents\arquivo.txt
```

O resultado irá mostrar o conteúdo do arquivo de texto diretamente no console.

## Profundidade:
Antes do PowerShell, os programadores costumavam utilizar comandos mais complexos para ler arquivos de texto, como o comando "type" no prompt de comando do Windows ou o comando "cat" no Linux. O PowerShell facilitou esse processo com o uso do cmdlet Get-Content, que é mais simples e eficiente. 

Existem também outras formas de ler arquivos de texto em PowerShell, como utilizando o objeto StreamReader da biblioteca .NET Framework ou o cmdlet Select-String. Cada opção possui suas próprias vantagens e pode ser usada de acordo com a necessidade do programador.

## Veja também:
Para saber mais sobre leitura de arquivos de texto em PowerShell, confira os links abaixo:

- [Documentação oficial do Get-Content](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content)
- [Exemplos de uso do Get-Content](https://adamtheautomator.com/powershell-get-content/)
- [Outras formas de ler arquivos de texto em PowerShell](https://www.computerperformance.co.uk/powershell/streamreader/)
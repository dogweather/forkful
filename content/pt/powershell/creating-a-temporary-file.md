---
title:                "Criando um arquivo temporário"
html_title:           "PowerShell: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Criar um arquivo temporário é o processo de criar um arquivo que será usado temporariamente para armazenar informações em um programa. Programadores geralmente fazem isso para economizar espaço de memória e aumentar a eficiência do programa.

## Como fazer:
No PowerShell, você pode criar um arquivo temporário usando o cmdlet `New-TemporaryFile`. Aqui está um exemplo de como usá-lo:

```PowerShell
# Criar um arquivo temporário e atribuir à variável $tempFile
$tempFile = New-TemporaryFile

# Adicionar conteúdo ao arquivo temporário
Add-Content -Path $tempFile -Value "Este é um arquivo temporário"

# Ver o conteúdo do arquivo temporário
Get-Content -Path $tempFile

# Remover o arquivo temporário
Remove-Item -Path $tempFile
```

Output:

```PowerShell
Este é um arquivo temporário
```

## Mergulho profundo:
Criar arquivos temporários é uma prática comum na programação e remonta aos primórdios da computação. Existem outras formas de armazenar informações temporariamente, como o uso de variáveis, mas criar arquivos temporários pode ser mais eficiente em alguns casos, especialmente quando se trabalha com grandes quantidades de dados. Ao criar um arquivo temporário, é importante lembrar de removê-lo após o uso para evitar a ocupação desnecessária de espaço de armazenamento.

## Veja também:
- Documentação oficial do cmdlet `New-TemporaryFile`: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/new-temporaryfile?view=powershell-7.1
- Artigo sobre como criar e usar arquivos temporários no PowerShell: https://www.howtogeek.com/682770/how-to-create-and-use-temporary-files-in-powershell/
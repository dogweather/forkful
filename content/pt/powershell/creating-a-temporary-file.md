---
title:                "Criando um arquivo temporário"
html_title:           "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O quê & Por quê?
Criar um arquivo temporário significa gerar um arquivo que será usado apenas enquanto o programa estiver em execução. Programadores fazem isso para armazenar dados intermediários sem usar a memória.

## Como fazer:
É fácil no PowerShell. Aqui está um código para criar um arquivo temporário:

```PowerShell
# Criar um arquivo temporário
$tempFile = [System.IO.Path]::GetTempFileName()
```

Quando você executa isso, ele retorna o nome do arquivo temporário gerado:

```PowerShell
C:\Users\SeuNome\AppData\Local\Temp\tmpFFFF.tmp
```

Você pode escrever nesse arquivo temporário assim:

```PowerShell
"Olá, código temporário!" | Out-File -FilePath $tempFile
```

Depois disso, lendo o conteúdo do arquivo teríamos:

```PowerShell
Get-Content -Path $tempFile
```

Isso retorna 'Olá, código temporário!'

Lembre-se de limpar e excluir o arquivo temporário após o uso!

```PowerShell
Remove-Item -Path $tempFile
```

## Aprofundando
Os arquivos temporários têm sido utilizados por anos como uma maneira fácil de armazenar dados de curto prazo. Existem alternativas, como armazenamento em memória (mais rápido, mas limitado pelo tamanho da memória) e bancos de dados (mais robustos, mas mais complicados).

Ao trabalhar com arquivos temporários no PowerShell, você está realmente interagindo com o sistema operacional subjacente. O PowerShell apenas fornece uma maneira fácil de fazer isso.

## Ver também
Confira esses links para obter mais informações detalhadas sobre o trabalho com arquivos temporários:

1. Documentos do Microsoft PowerShell: https://docs.microsoft.com/pt-br/powershell/
2. Artigo sobre arquivos temporários: https://www.codeproject.com/Articles/43438/Understanding-Temporary-Files
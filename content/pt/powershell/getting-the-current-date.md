---
title:                "Obtendo a data atual"
date:                  2024-01-20T15:16:20.604855-07:00
html_title:           "Bash: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Obter a data atual no PowerShell é pegar o momento exato em que o código é executado. Programadores fazem isso para registrar eventos, comparar datas ou simplesmente apresentar a data e hora para o usuário.

## Como Fazer:
Usar o PowerShell para obter a data e a hora atuais é simples. O cmdlet `Get-Date` é seu amigo aqui. Vamos dar uma olhada em alguns exemplos:

```PowerShell
# Obter a data e a hora atual
Get-Date

# Formatar a saída
Get-Date -Format "dd-MM-yyyy HH:mm:ss"

# Salvar a data e a hora numa variável
$dataAtual = Get-Date
```

Exemplo de saída:

```
Quinta-feira, 23 de Março de 2023 14:23:45
23-03-2023 14:23:45
```

## Mergulho Profundo:
Historicamente, `Get-Date` é um cmdlet disponível desde as primeiras versões do PowerShell, fornecendo funcionalidade básica para trabalhar com datas e horas. 

Alternativas incluem o uso de métodos .NET se precisar de funcionalidades mais complexas:

```PowerShell
# Usando o objeto DateTime do .NET
[DateTime]::Now

# Usando ToUniversalTime para obter a hora UTC
(Get-Date).ToUniversalTime()
```

Quando se fala em implementação, `Get-Date` se baseia na classe `DateTime` do .NET Framework, o que significa que você tem acesso a todos os métodos e propriedades desse tipo robusto se precisar expandir suas operações com datas e horas.

## Veja Também:
- [Documentação oficial do cmdlet Get-Date](https://docs.microsoft.com/pt-br/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.2)
- [C# DateTime Struct (em inglês)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)

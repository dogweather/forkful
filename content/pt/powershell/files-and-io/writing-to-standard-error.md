---
title:                "Escrevendo para o erro padrão"
aliases:
- /pt/powershell/writing-to-standard-error.md
date:                  2024-02-03T19:34:06.891507-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escrevendo para o erro padrão"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?

Escrever para o erro padrão (stderr) no PowerShell envolve enviar mensagens de erro ou diagnósticos diretamente para o stream stderr, distinto do stream de saída padrão (stdout). Essa separação permite um controle mais preciso sobre a saída de um script, possibilitando aos desenvolvedores direcionar mensagens normais e de erro para destinos diferentes, o que é fundamental para o tratamento de erros e registro de logs.

## Como Fazer:

O PowerShell simplifica o processo de escrever para stderr através do uso do cmdlet `Write-Error` ou direcionando a saída para o método `$host.ui.WriteErrorLine()`. No entanto, para a redireção direta de stderr, você pode preferir usar métodos .NET ou a redireção de descritor de arquivo oferecida pelo próprio PowerShell.

**Exemplo 1:** Usando `Write-Error` para escrever uma mensagem de erro para stderr.

```powershell
Write-Error "Esta é uma mensagem de erro."
```

Saída para stderr:
```
Write-Error: Esta é uma mensagem de erro.
```

**Exemplo 2:** Usando `$host.ui.WriteErrorLine()` para escrita direta em stderr.

```powershell
$host.ui.WriteErrorLine("Escrita direta em stderr.")
```

Saída para stderr:
```
Escrita direta em stderr.
```

**Exemplo 3:** Usando métodos .NET para escrever em stderr.

```powershell
[Console]::Error.WriteLine("Usando método .NET para stderr")
```

Saída deste método:
```
Usando método .NET para stderr
```

**Exemplo 4:** Redirecionando a saída de erro usando o descritor de arquivo `2>`.

Descritores de arquivos no PowerShell podem redirecionar diferentes streams. Para stderr, o descritor de arquivo é `2`. Aqui está um exemplo de redirecionamento de stderr para um arquivo chamado `error.log` enquanto executa um comando que gera um erro.

```powershell
Get-Item NonExistentFile.txt 2> error.log
```

Este exemplo não produz saída no console, mas gera um arquivo `error.log` no diretório atual contendo a mensagem de erro ao tentar acessar um arquivo que não existe.

Concluindo, o PowerShell oferece múltiplos métodos para escrever e gerenciar efetivamente a saída de erro, permitindo estratégias sofisticadas de tratamento de erros e registro de logs em scripts e aplicações.

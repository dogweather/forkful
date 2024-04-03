---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:26.607005-07:00
description: "Como fazer: O PowerShell fornece cmdlets simples para obter a data e\
  \ a hora. O cmdlet `Get-Date` \xE9 a principal ferramenta para esse prop\xF3sito.\
  \ Ele pode\u2026"
lastmod: '2024-03-13T22:44:46.807339-06:00'
model: gpt-4-0125-preview
summary: O PowerShell fornece cmdlets simples para obter a data e a hora.
title: Obtendo a data atual
weight: 29
---

## Como fazer:
O PowerShell fornece cmdlets simples para obter a data e a hora. O cmdlet `Get-Date` é a principal ferramenta para esse propósito. Ele pode retornar a data e hora completa, que você pode formatar ou manipular de acordo com suas necessidades.

```powershell
# Obter a data e hora atual
Get-Date
```

**Saída de exemplo:**

```
Tuesday, September 5, 2023 9:46:02 AM
```

Você também pode formatar a saída para exibir apenas as informações de que precisa, como apenas a data ou apenas a hora.

```powershell
# Obter apenas a data atual em um formato específico
Get-Date -Format "yyyy-MM-dd"
```

**Saída de exemplo:**

```
2023-09-05
```

```powershell
# Obter apenas a hora atual
Get-Date -Format "HH:mm:ss"
```

**Saída de exemplo:**

```
09:46:02
```

### Usando a Classe .NET
O PowerShell permite o acesso direto às classes .NET, oferecendo uma maneira alternativa de trabalhar com datas e horas.

```powershell
# Usando a classe .NET DateTime para obter a data e hora atual
[System.DateTime]::Now
```

**Saída de exemplo:**

```
Tuesday, September 5, 2023 9:46:02 AM
```

Para a hora UTC:

```powershell
# Usando a classe .NET DateTime para obter a data e hora UTC atual
[System.DateTime]::UtcNow
```

**Saída de exemplo:**

```
Tuesday, September 5, 2023 1:46:02 PM
```

Estes comandos e classes fornecem opções poderosas e flexíveis para trabalhar com datas e horas no PowerShell, essenciais para muitas tarefas de script e automação.

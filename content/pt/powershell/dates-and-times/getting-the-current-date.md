---
title:                "Obtendo a data atual"
aliases:
- /pt/powershell/getting-the-current-date.md
date:                  2024-02-03T19:10:26.607005-07:00
model:                 gpt-4-0125-preview
simple_title:         "Obtendo a data atual"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Quê & Por Quê?

Obter a data atual no PowerShell é sobre recuperar a data e hora atual do sistema. Esta operação é fundamental para tarefas como registrar, cronometrar operações ou tomar decisões baseadas em datas. Programadores usam essa capacidade para rastrear eventos, agendar tarefas e lidar com lógica específica de datas em scripts e aplicações.

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

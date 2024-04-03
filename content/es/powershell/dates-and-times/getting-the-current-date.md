---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:21.865010-07:00
description: "C\xF3mo hacerlo: PowerShell proporciona cmdlets sencillos para obtener\
  \ la fecha y la hora. El cmdlet `Get-Date` es la herramienta primaria para este\u2026"
lastmod: '2024-03-13T22:44:59.302527-06:00'
model: gpt-4-0125-preview
summary: PowerShell proporciona cmdlets sencillos para obtener la fecha y la hora.
title: Obteniendo la fecha actual
weight: 29
---

## Cómo hacerlo:
PowerShell proporciona cmdlets sencillos para obtener la fecha y la hora. El cmdlet `Get-Date` es la herramienta primaria para este propósito. Puede devolver la fecha y hora completas, que puedes formatear o manipular según tus necesidades.

```powershell
# Obtener la fecha y hora actuales
Get-Date
```

**Salida de muestra:**

```
Martes, 5 de septiembre de 2023 9:46:02 AM
```

También puedes formatear la salida para mostrar solo la información que necesitas, como solo la fecha o solo la hora.

```powershell
# Obtener solo la fecha actual en un formato específico
Get-Date -Format "yyyy-MM-dd"
```

**Salida de muestra:**

```
2023-09-05
```

```powershell
# Obtener solo la hora actual
Get-Date -Format "HH:mm:ss"
```

**Salida de muestra:**

```
09:46:02
```

### Usando la clase .NET
PowerShell permite el acceso directo a las clases .NET, ofreciendo una manera alternativa de trabajar con fechas y horas.

```powershell
# Usando la clase .NET DateTime para obtener la fecha y hora actuales
[System.DateTime]::Now
```

**Salida de muestra:**

```
Martes, 5 de septiembre de 2023 9:46:02 AM
```

Para la hora UTC:

```powershell
# Usando la clase .NET DateTime para obtener la fecha y hora UTC actuales
[System.DateTime]::UtcNow
```

**Salida de muestra:**

```
Martes, 5 de septiembre de 2023 1:46:02 PM
```

Estos comandos y clases proporcionan opciones poderosas y flexibles para trabajar con fechas y horas en PowerShell, esenciales para muchas tareas de scripting y automatización.

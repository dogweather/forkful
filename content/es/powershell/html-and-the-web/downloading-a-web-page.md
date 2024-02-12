---
title:                "Descargando una página web"
aliases:
- /es/powershell/downloading-a-web-page/
date:                  2024-01-20T17:44:50.761976-07:00
model:                 gpt-4-1106-preview
simple_title:         "Descargando una página web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Descargar una página web significa traer el contenido de una página de internet a tu PC local. Los programadores hacen esto para análisis, pruebas, backup, o para automatizar tareas que dependen de información web.

## Cómo hacerlo:
```PowerShell
# Usando Invoke-WebRequest para guardar el contenido en una variable
$paginaWeb = Invoke-WebRequest -Uri "https://www.example.com"
# Para guardar directamente en un archivo
Invoke-WebRequest -Uri "https://www.example.com" -OutFile "pagina.html"

# Muestra del contenido HTML de la página
$paginaWeb.Content
```

## Deep Dive
En PowerShell, `Invoke-WebRequest` es el cmdlet principal para interactuar con la web. Lanzado con la versión 3.0, fue un gran avance para la automatización web en Windows. Alternativamente, puedes usar `curl` o `wget` en PowerShell 6+, que son comandos conocidos en ambientes Unix. Estos cmdlets te dejan personalizar headers HTTP, métodos de solicitud (GET, POST), y manejar sesiones web.

Implementar la descarga de una página web con PowerShell es poderoso porque puedes integrarla con otras tareas administrativas. Por ejemplo, podrías descargar un sitio y buscar ciertas palabras, o verificar cambios regulares en el contenido para alertas automáticas.

## Ver También
- [Invoke-WebRequest documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [HttpWebRequest class in .NET](https://docs.microsoft.com/en-us/dotnet/api/system.net.httpwebrequest) para cuando necesitas algo más avanzado y estás dispuesto a escribir algo de C# en tu script PowerShell.

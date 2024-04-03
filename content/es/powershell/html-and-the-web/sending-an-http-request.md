---
date: 2024-01-20 18:00:25.981787-07:00
description: "Enviar una solicitud HTTP permite a tu programa hablar con otros sistemas\
  \ a trav\xE9s de la web; es como enviar un mensaje que pide datos o que ejecute\
  \ una\u2026"
lastmod: '2024-03-13T22:44:59.289244-06:00'
model: gpt-4-1106-preview
summary: "Enviar una solicitud HTTP permite a tu programa hablar con otros sistemas\
  \ a trav\xE9s de la web; es como enviar un mensaje que pide datos o que ejecute\
  \ una acci\xF3n."
title: Enviando una solicitud http
weight: 44
---

## ¿Qué y por qué?
Enviar una solicitud HTTP permite a tu programa hablar con otros sistemas a través de la web; es como enviar un mensaje que pide datos o que ejecute una acción. Los programadores lo hacen para interactuar con servicios web, APIs o para comunicarse entre distintas partes de una aplicación distribuida.

## Cómo hacerlo:
PowerShell facilita el envío de solicitudes HTTP con los cmdlets `Invoke-WebRequest` e `Invoke-RestMethod`. Aquí te muestro cómo usarlos.

```PowerShell
# Obtén contenido de una página web con Invoke-WebRequest
$response = Invoke-WebRequest -Uri "https://api.example.com/data"
$response.Content

# Usando Invoke-RestMethod para obtener datos en formato JSON y convertirlos directamente en objetos de PowerShell
$data = Invoke-RestMethod -Uri "https://api.example.com/data" -Method Get
$data
```

Ejemplo de salida para `Invoke-WebRequest`:
```
<!DOCTYPE html>
<html>
<body>
    <h1>Ejemplo de Respuesta</h1>
</body>
</html>
```

Ejemplo de salida para `Invoke-RestMethod` (suponiendo que la API devuelve JSON):
```
name        : PowerShell Ejemplo
description : Esto es un objeto devuelto por Invoke-RestMethod
id          : 12345
```

Usa `Invoke-RestMethod` si trabajas con APIs RESTful y necesitas procesar objetos; usa `Invoke-WebRequest` para datos crudos o interacciones web más detalladas.

## Profundización:
Antes de PowerShell, se usaban otros lenguajes y herramientas como cURL o herramientas específicas de cada lenguaje para enviar solicitudes HTTP. 

PowerShell facilitó el proceso con cmdlets específicos. `Invoke-RestMethod` se introdujo en PowerShell 3.0 y fue diseñado para APIs REST, ya que puede interpretar formatos estándar como JSON y XML, transformándolos en objetos de PowerShell.

`Invoke-WebRequest` es más versátil y te da acceso a más detalles de la respuesta, como los headers HTTP, status codes, y la posibilidad de interactuar con sesiones web.

Recuerda manejar errores con try/catch y validar los códigos de estado (status codes) de las respuestas HTTP para asegurar que tu solicitud fue procesada correctamente.

## Ver También:
- [Invoke-WebRequest documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [Invoke-RestMethod documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [About Try, Catch, and Finally in PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-exceptions)
- [Understanding REST APIs](https://www.redhat.com/en/topics/api/what-is-a-rest-api)

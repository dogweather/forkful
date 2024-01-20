---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Elixir: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

El borrado de caracteres que coinciden con un patrón se refiere a localizar y eliminar ciertos caracteres de una cadena de texto en la programación con PowerShell. Los programadores lo necesitan para limpiar cadenas de datos no necesarios, errores de sintaxis y para el procesamiento de texto más eficiente.

## ¿Cómo se hace?

En PowerShell, puedes usar `-replace` para eliminar caracteres que coincidan con un patrón. Use una cadena vacía (' ') como segundo argumento para borrar.

Aquí te muestro cómo hacerlo: 

```PowerShell
$val = "HolaMundo123"
$val = $val -replace '[0-9]', '' # borra todos los números
echo $val
```
Salida: 

```PowerShell
HolaMundo
```

## Un vistazo más profundo

PowerShell, antiguamente conocido como Monad, se lanzó en 2006 como un shell de scripting basado en .NET. La necesidad de manipular texto (como borrar caracteres siguiendo patrones) ha estado ahí desde sus inicios.

Como alternativas, también puedes usar el método `.Trim()`, aunque este último solo funciona para borrar caracteres del principio o final de una cadena, no en medio. 

Respecto a los detalles de implementación, cuando usas `-replace`, PowerShell usa internamente la clase Regex de .NET, la cual es muy potente pero también puede ser lenta para patrones complicados.

## Ver además

1. [Documentación oficial de PowerShell](https://docs.microsoft.com/es-es/powershell/)
2. [Regex Class](https://docs.microsoft.com/es-es/dotnet/api/system.text.regularexpressions.regex?view=net-5.0) de .NET

Recuerda, practicar es la mejor manera de aprender. ¡Pon en práctica estos ejemplos y ve cómo funcionan!
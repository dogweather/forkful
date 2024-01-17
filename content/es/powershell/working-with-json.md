---
title:                "Trabajando con json"
html_title:           "PowerShell: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Trabajar con JSON es una forma eficiente y sencilla de almacenar y transmitir datos estructurados en formato de texto. Como programadores, a menudo nos encontramos en la necesidad de leer, escribir o manipular datos en formato JSON para diversas tareas, como integrar sistemas o trabajar con APIs.

## ¿Cómo hacerlo?
PowerShell ofrece diferentes cmdlets para trabajar con JSON de manera fácil y rápida. A continuación, te mostramos algunos ejemplos y su salida correspondiente en formato PowerShell:

Obtener datos de un archivo JSON:

```PowerShell
Get-Content .\datos.json | ConvertFrom-Json
```

``` PowerShell
Nombre           : Juan
Apellido         : Pérez
Edad             : 30
Ciudad           : Madrid
```

Crear un objeto JSON a partir de un objeto de PowerShell:

```PowerShell
$datos = [PSCustomObject]@{
    Nombre = 'María'
    Apellido = 'García'
    Edad = 25
    Ciudad = 'Barcelona'
} | ConvertTo-Json
```

``` PowerShell
"{\"Nombre\":\"María\",\"Apellido\":\"García\",\"Edad\":25,\"Ciudad\":\"Barcelona\"}"
```

Agregar un nuevo elemento a un archivo JSON:

```PowerShell
$cliente = [PSCustomObject]@{
    Nombre = 'Pedro'
    Apellido = 'López'
    Edad = 35
    Ciudad = 'Valencia'
}
$datos | ConvertFrom-Json | ForEach-Object {
    $_.Clientes += $cliente
} | ConvertTo-Json | Out-File .\datos.json
```

``` PowerShell
{
    "Nombre": "Juan",
    "Apellido": "Pérez",
    "Edad": 30,
    "Ciudad": "Madrid",
    "Clientes": [
        {
            "Nombre": "María",
            "Apellido": "García",
            "Edad": 25,
            "Ciudad": "Barcelona"
        },
        {
            "Nombre": "Pedro",
            "Apellido": "López",
            "Edad": 35,
            "Ciudad": "Valencia"
        }
    ]
}
```

## Profundizando
En los últimos años, JSON se ha convertido en un formato de datos muy popular debido a su legibilidad y facilidad para ser interpretado por diferentes lenguajes de programación. Antes de la aparición de JSON, el formato más común para transmitir datos era XML. Sin embargo, JSON es más sencillo y eficiente, ya que utiliza menos caracteres y su estructura es más clara.

Otra alternativa para trabajar con JSON en PowerShell es utilizando módulos externos, como "ConvertFrom-JsonCmdlet" que ofrece más funcionalidades y opciones de manejo de errores.

En cuanto a la implementación, PowerShell utiliza el formato JSON para almacenar objetos de manera similar a como lo hace en el formato XML, pero con una sintaxis más simple. Además, también es posible trabajar con datos multilínea en PowerShell utilizando el backtick (`) para separar las líneas del objeto JSON.

## Ver también
Si quieres saber más sobre JSON y cómo trabajar con él en PowerShell, estas son algunas fuentes de información que pueden ser de ayuda:

- [Documentación oficial de Microsoft sobre ConvertFrom-Json](https://docs.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Utility/ConvertFrom-Json?view=powershell-7.1)
- [Tutorial en español sobre el manejo de JSON en PowerShell](https://www.jesusninoc.com/2017/07/02/convertir-objetos-json-a-objetos-de-powershell/)
- [Módulo externo "ConvertFrom-JsonCmdlet"](https://www.powershellgallery.com/packages/ConvertFrom-JsonCmdlet/3.1.0/Content/ConvertFrom-Json/about_ConvertFrom-JsonCmdlet_help.md)
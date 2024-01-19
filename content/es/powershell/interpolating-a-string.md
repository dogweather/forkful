---
title:                "Interpolando una cadena de texto"
html_title:           "Haskell: Interpolando una cadena de texto"
simple_title:         "Interpolando una cadena de texto"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

### ¿Qué & Por qué?
La interpolación de cadenas implica insertar datos variables en una cadena de texto. Los programadores lo hacen porque aumenta la legibilidad y permite una manipulación de cadenas dinámica y eficiente. 

### Cómo hacerlo:
Aquí está el código de muestra que representa el uso de la interpolación de cadenas. Los bloques de código en PowerShell usan ```PowerShell...```

```PowerShell
$nombre = "Juan"
$saludo = "¡Hola, $nombre!"
echo $saludo
```

Esto imprimirá:

```PowerShell
¡Hola, Juan!
```

### Inmersión profunda
La interpolación de cadenas ha estado alrededor por mucho tiempo y proviene de lenguajes de programación como Perl y Unix Shell. Alternativamente, puedes usar la concatenación de cadenas, aunque no es tan eficiente ni tan legible como la interpolación. En PowerShell, la interpolación de cadenas se implementa mediante la inserción de la variable directamente en la cadena, con el valor de la variable reemplazado en tiempo de ejecución.

### Ver también
Para más información, consulta las referencias a continuación:

1. Documentación oficial de PowerShell: [Acerca de las cadenas](https://docs.microsoft.com/es-es/powershell/module/microsoft.powershell.core/about/about_strings)
2. Blog de Keith Dahlby: [Interpolación de cadenas en PowerShell](http://keith.gauch.net/2016/10/string-interpolation-in-powershell.html)
3. Overflow de pilas: [Interpolación de cadenas vs concatenación en PowerShell](https://stackoverflow.com/questions/37904682/powershell-string-interpolation-vs-concatenation).
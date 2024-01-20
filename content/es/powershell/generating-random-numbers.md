---
title:                "Generando números aleatorios"
html_title:           "Arduino: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Generar números aleatorios involucra la creación de números que no tienen un patrón predecible, cada número se genera de manera independiente de los demás. Los programadores los usan para introducir aleatoriedad en programas, generando resultados impredecibles en las pruebas de software, seguridad cibernética, y simulaciones.

## Cómo se hace:

En PowerShell, puedes generar números aleatorios usando el cmdlet Get-Random.

Generar un número aleatorio entre 0 y 100.

```PowerShell
Get-Random -Minimum 0 -Maximum 101
```

Salida de muestra:

```PowerShell
57
```

Generar un número decimal aleatorio.

```PowerShell
[decimal](Get-Random -Maximum 1)
```

Salida de muestra:

```PowerShell
0.2398776
```

## Hurgando un Poco

El generador de números aleatorios de PowerShell utiliza internamente un generador de congruencia lineal, el cual produce una secuencia de números pseudorromos. 

Las alternativas a Get-Random incluyen el uso de la función de tiempo del sistema para generar un "número aleatorio" a partir de la marca de tiempo más baja, pero los números generados de esta forma son a menudo más predecibles.

La implementación actual en PowerShell utiliza System.Random del .Net Framework. Esta clase mantiene una semilla que cambia cada vez que se genera un número, lo que asegura que la misma secuencia de números no se repita.

## Ver También

- Doc oficial del cmdlet Get-Random: https://docs.microsoft.com/es-es/powershell/module/microsoft.powershell.utility/get-random?view=powershell-7.1
- Conoce más sobre el Generador de Congruencia Lineal: https://es.wikipedia.org/wiki/Generador_lineal_congruencial
- Clase System.Random de .NET: https://docs.microsoft.com/es-es/dotnet/api/system.random?view=net-6.0
---
aliases:
- /es/powershell/working-with-complex-numbers/
date: 2024-01-26 04:43:57.711042-07:00
description: "Los n\xFAmeros complejos, aquellos con una parte real y una parte imaginaria\
  \ (como 3 + 4i), son vitales en campos como la ingenier\xEDa, f\xEDsica y ciencia\
  \ de\u2026"
lastmod: 2024-02-18 23:09:10.206801
model: gpt-4-0125-preview
summary: "Los n\xFAmeros complejos, aquellos con una parte real y una parte imaginaria\
  \ (como 3 + 4i), son vitales en campos como la ingenier\xEDa, f\xEDsica y ciencia\
  \ de\u2026"
title: "Trabajando con n\xFAmeros complejos"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Los números complejos, aquellos con una parte real y una parte imaginaria (como 3 + 4i), son vitales en campos como la ingeniería, física y ciencia de datos. Los programadores los utilizan para simulaciones, procesamiento de señales y resolver tipos específicos de problemas matemáticos.

## Cómo hacerlo:
PowerShell no tiene soporte incorporado para números complejos, así que o bien desarrollas tu propia solución o utilizas `System.Numerics.Complex` de .NET.

```PowerShell
# Vamos a crear números complejos usando .NET
[Reflection.Assembly]::LoadWithPartialName("System.Numerics") | Out-Null

# Crear números complejos
$complex1 = [System.Numerics.Complex]::new(3, 4) # 3 + 4i
$complex2 = [System.Numerics.Complex]::new(1, 2) # 1 + 2i

# Sumar dos números complejos
$sum = [System.Numerics.Complex]::Add($complex1, $complex2) # 4 + 6i

# Multiplicar dos números complejos
$product = [System.Numerics.Complex]::Multiply($complex1, $complex2) # -5 + 10i

# Mostrar los resultados
"Suma: $sum"
"Producto: $product"
```
Salida:
```
Suma: (4, 6)
Producto: (-5, 10)
```

## Profundización
Los números complejos fueron desarrollados en el siglo 16 para resolver ecuaciones que no tenían soluciones en el ámbito de los números reales. Ahora son una piedra angular de la matemática moderna.

La dependencia de PowerShell en .NET para el soporte de números complejos significa que el rendimiento es sólido. Las alternativas incluyen bibliotecas de terceros u otros lenguajes de programación como Python, donde los números complejos son un tipo de datos nativo.

## Ver también
- [Estructura System.Numerics.Complex](https://docs.microsoft.com/en-us/dotnet/api/system.numerics.complex)
- [Aritmética de números complejos en Python](https://docs.python.org/3/library/cmath.html)

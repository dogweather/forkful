---
date: 2024-01-20 17:57:49.180559-07:00
description: "Buscar y reemplazar texto es sencillo: es encontrar una cadena de texto\
  \ y cambiarla por otra. Los programadores lo hacen para actualizar datos, corregir\u2026"
lastmod: 2024-02-19 22:05:17.275075
model: gpt-4-1106-preview
summary: "Buscar y reemplazar texto es sencillo: es encontrar una cadena de texto\
  \ y cambiarla por otra. Los programadores lo hacen para actualizar datos, corregir\u2026"
title: Buscando y reemplazando texto
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Buscar y reemplazar texto es sencillo: es encontrar una cadena de texto y cambiarla por otra. Los programadores lo hacen para actualizar datos, corregir errores o modificar código rápidamente.

## Cómo Hacerlo:

Elixir es conciso y poderoso. Aquí va un ejemplo sencillo:

```elixir
texto = "Quiero tres platos de arroz."
nuevo_texto = String.replace(texto, "tres", "cuatro")

IO.puts nuevo_texto
```

Salida:
```
Quiero cuatro platos de arroz.
```

¿Necesitas algo más avanzado? Regex al rescate:

```elixir
texto = "Elixir mola mucho. Elixir es increíble."
regex = ~r/Elixir/
nuevo_texto = String.replace(texto, regex, "Phoenix")

IO.puts nuevo_texto
```

Salida:
```
Phoenix mola mucho. Phoenix es increíble.
```

## Inmersión Profunda:

Originalmente, esta funcionalidad viene de los editores de texto y procesadores de palabra. Programas como `sed` en Unix son legendarios por su potencia aquí.

En Elixir, se puede hacer de forma más segura y legible gracias a patrones como la Programación Funcional y el uso de Regex. Pero cuidado, el abuso de Regex puede complicar el código. Y si no quieres usar la librería estándar, existen paquetes como `regex` que ofrecen más flexibilidad.

Detalles de implementación: `String.replace/4` es nuestra navaja suiza aquí, con parámetros para opciones avanzadas como reemplazos globales o limitados. Usa el módulo `Regex` bajo el capó para los patrones complejos.

## Ver También:

- [Elixir School](https://elixirschool.com/es/lessons/basics/strings/#reemplazo) para una introducción práctica y sencilla.
- [Elixir String Docs](https://hexdocs.pm/elixir/String.html) para referencia detallada de funciones.

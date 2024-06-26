---
date: 2024-01-20 17:57:49.180559-07:00
description: "C\xF3mo Hacerlo: Elixir es conciso y poderoso. Aqu\xED va un ejemplo\
  \ sencillo."
lastmod: '2024-03-13T22:44:58.686432-06:00'
model: gpt-4-1106-preview
summary: Elixir es conciso y poderoso.
title: Buscando y reemplazando texto
weight: 10
---

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

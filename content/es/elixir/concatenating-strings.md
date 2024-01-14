---
title:    "Elixir: Uniendo cadenas de texto"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## ¿Por qué concatenar cadenas en Elixir?

Concatenar cadenas en Elixir puede ser útil cuando se están manipulando datos y se necesita combinarlos en una sola cadena. Por ejemplo, se puede utilizar para mostrar información al usuario, crear nombres de archivo dinámicos, o incluso para construir consultas de base de datos.

## Cómo hacerlo:

Para concatenar cadenas en Elixir, se puede utilizar el operador `<>` o la función `String.concat/2`.

```Elixir
# Utilizando el operador <>
"Puedo " <> "concatenar " <> "cadenas"

# Utilizando la función String.concat/2
String.concat(["Puedo ", "concatenar ", "cadenas"])
```

**Salida:**
"Puedo concatenar cadenas"

También se puede utilizar la función `<>` para concatenar cadenas múltiples en una sola línea:

```Elixir
"Puedo " <> "concatenar " <> "más " <> "de " <> "dos " <> "cadenas" 
```

**Salida:**
"Puedo concatenar más de dos cadenas"

## Profundizando en la concatenación de cadenas:

Cuando se concatenan cadenas en Elixir, es importante recordar que se está creando una nueva cadena y no se está modificando la cadena original. Además, Elixir no tiene un operador de concatenación `+=` como en otros lenguajes de programación.

También es importante tener en cuenta que se pueden concatenar diferentes tipos de datos, no solo cadenas. Por ejemplo:

```Elixir
"Puedo " <> 123 <> "concatenar " <> true <> "cadenas con otros tipos de datos"
```

**Salida:**
"Puedo 123 concatenar true cadenas con otros tipos de datos"

## Ver también:

- [La documentación de Elixir para la función `String.concat/2`](https://hexdocs.pm/elixir/String.html#concat/2)
- [Un tutorial sobre manipulación de cadenas en Elixir](https://www.learnelixir.tv/blog/2016/01/03/elixir-string-manipulation)
- [Un artículo sobre tipos de datos y funciones en Elixir](https://medium.com/@dennis_richardt/introduction-to-elixir-types-and-functions-ddbe83fbdf40)
---
date: 2024-01-26 03:39:01.152430-07:00
description: "Eliminar las comillas de una cadena significa deshacerse de esos envoltorios\
  \ extra para obtener el texto limpio dentro. Los programadores hacen esto para\u2026"
lastmod: '2024-03-13T22:44:58.689516-06:00'
model: gpt-4-0125-preview
summary: Eliminar las comillas de una cadena significa deshacerse de esos envoltorios
  extra para obtener el texto limpio dentro.
title: Eliminando comillas de una cadena
weight: 9
---

## Cómo hacerlo:
Elixir no tiene una función integrada para 'eliminar comillas', pero es muy fácil crear la propia con el emparejamiento de patrones o funciones de `String`. Mira estos fragmentos:

```elixir
# Usando emparejamiento de patrones
def unquote_string("\"" <> quoted_string <> "\""), do: quoted_string
def unquote_string("'" <> quoted_string <> "'"), do: quoted_string
def unquote_string(quoted_string), do: quoted_string

# Uso de muestra
unquote_string("\"Hola, Mundo!\"") # => "Hola, Mundo!"
unquote_string("'Hola, Mundo!'")   # => "Hola, Mundo!"

# Usando String.trim/1
def unquote_string(string), do: String.trim(string, "'\"")

# Uso de muestra
unquote_string("\"Hola, Mundo!\"") # => "Hola, Mundo!"
unquote_string("'Hola, Mundo!'")   # => "Hola, Mundo!"
```

La salida para ambos métodos será:
```
"Hola, Mundo!"
```

## Estudio Profundo
Antiguamente, las comillas en las cadenas eran un campo minado: manéjalas mal y boom, errores de sintaxis o agujeros de seguridad. En Elixir, el emparejamiento de patrones trata tus cadenas como bloques de Lego, permitiéndote desmontar y reconstruir con precisión. Su robusto módulo `String` también es útil, eliminando flexiblemente las comillas con funciones `trim`. ¿Las alternativas? Las expresiones regulares pueden deshacerse de las comillas, y las bibliotecas externas podrían ofrecer más potencia si necesitas más que una simple eliminación.

## Ver También
Profundiza con estos:
- [Módulo String de Elixir](https://hexdocs.pm/elixir/String.html)
- [Aprende más sobre el emparejamiento de patrones en Elixir](https://elixir-lang.org/getting-started/pattern-matching.html)
- [Expresiones regulares en Elixir (módulo Regex)](https://hexdocs.pm/elixir/Regex.html)

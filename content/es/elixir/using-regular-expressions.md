---
title:                "Uso de expresiones regulares"
html_title:           "Arduino: Uso de expresiones regulares"
simple_title:         "Uso de expresiones regulares"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Las expresiones regulares son patrones usados para encontrar coincidencias y manipular texto. Los programadores las usan por su potencia y flexibilidad para validar, buscar y editar cadenas de caracteres de manera eficiente.

## Cómo hacerlo:
```elixir
# Definir una expresión regular
regex = ~r/elixir/

# Encontrar una coincidencia en una cadena
"Me encanta programar en Elixir". =~ regex
# Resultado: true

# Extraer todas las coincidencias
Regex.scan(~r/\d/, "Año 2023: el futuro del Elixir")
# Resultado: [["2"], ["0"], ["2"], ["3"]]

# Reemplazar texto que coincida
Regex.replace(~r/\s/, "espacio por guión", "-")
# Resultado: "espacio-por-guión"
```

## En Profundidad:
Históricamente, las expresiones regulares vienen de la teoría de autómatas y lenguajes formales. En Elixir, las regex se implementan mediante la biblioteca `:re`, que es una API de Erlang para el motor de expresiones regulares PCRE (Perl Compatible Regular Expressions). Como alternativas a las regex, a veces se pueden usar funciones del módulo `String` para tareas simples, aunque con menos potencia.

## Ver También:
- [Documentación de `Regex` en Elixir](https://hexdocs.pm/elixir/Regex.html)
- [Guía de inicio rápido de expresiones regulares](https://www.regular-expressions.info/quickstart.html)
- [PCRE(Perl Compatible Regular Expressions)](http://www.pcre.org/)
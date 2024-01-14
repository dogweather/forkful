---
title:    "Elixir: Uso de expresiones regulares"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

¡Hola a todos los lectores de programación Elixir en español! En este post, vamos a hablar sobre una herramienta muy útil en la programación: las expresiones regulares.

## Porqué

Las expresiones regulares son una forma poderosa de buscar y manipular texto. Son especialmente útiles para tareas como validar datos de entrada o buscar patrones en un texto. Con las expresiones regulares, puedes realizar búsquedas más precisas y eficientes en tus programas.

## Cómo

Para utilizar expresiones regulares en Elixir, primero debes importar el módulo `Regex` en tu archivo de código.

```
import Regex
```

Luego, puedes utilizar el operador `=~` para buscar una expresión regular en una cadena de texto. Por ejemplo, si queremos encontrar todas las palabras que empiezan con la letra "a" en una lista, podemos hacerlo de la siguiente manera:

```
palabras = ["amor", "azul", "oso", "casa"]
for palabra <- palabras do
  if palabra =~ ~r/^a/ do
    IO.puts palabra
  end
end
```

Este código imprimirá "amor" y "azul", ya que son las únicas palabras que cumplen con la expresión regular (`~r/^a/`).

También puedes usar expresiones regulares para reemplazar parte de una cadena de texto con `String.replace/4` y `Regex.replace/3`. Por ejemplo, si queremos cambiar la fecha en un formato DD/MM/YYYY a MM/DD/YYYY, podemos hacerlo así:

```
texto = "Hoy es 01/07/2021."
nuevo_texto = texto
  |> Regex.replace(~r/(\d{2})\/(\d{2})\/(\d{4})/, "\\2/\\1/\\3")
IO.puts nuevo_texto
```

La salida será "Hoy es 07/01/2021.", ya que hemos invertido el día y el mes en la fecha.

## Profundizando

Las expresiones regulares también tienen una gran cantidad de opciones y símbolos que puedes utilizar para buscar patrones más específicos. Por ejemplo, puedes utilizar `.` para representar cualquier caracter, `+` para indicar que el elemento anterior puede aparecer una o más veces, y `\d` para representar un dígito. Puedes encontrar una lista completa de estas opciones en la documentación oficial de Elixir.

Además, puedes utilizar grupos de captura en tus expresiones regulares para guardar partes específicas de una cadena de texto en variables. Estos grupos se pueden utilizar luego en reemplazos, lo que hace que el código sea aún más dinámico y reutilizable.

## Ver también

- [Documentación de Elixir sobre expresiones regulares](https://hexdocs.pm/elixir/Regex.html)
- [Tutorial de expresiones regulares en Elixir](https://medium.com/@grelsmag/tutorial-elixir-regex-268f7cc1d9d6)
- [Cheat sheet de expresiones regulares en Elixir](https://www.rexegg.com/regex-elixir.html)

¡Esperamos que este artículo te haya dado una mejor comprensión de cómo utilizar expresiones regulares en tus programas de Elixir! ¡Happy coding!
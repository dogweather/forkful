---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:49.177501-07:00
description: "Capitalizar una cadena implica convertir la primera letra de la cadena\
  \ en may\xFAscula mientras aseguramos que el resto de las letras est\xE9n en min\xFA\
  sculas.\u2026"
lastmod: '2024-03-13T22:44:58.684329-06:00'
model: gpt-4-0125-preview
summary: "Capitalizar una cadena implica convertir la primera letra de la cadena en\
  \ may\xFAscula mientras aseguramos que el resto de las letras est\xE9n en min\xFA\
  sculas."
title: Capitalizando una cadena de texto
weight: 2
---

## Cómo hacerlo:
Elixir provee una manera directa de capitalizar cadenas utilizando sus funciones integradas sin la necesidad de librerías de terceros. Aquí hay un ejemplo simple:

```elixir
string = "elixir programming"
capitalized_string = String.capitalize(string)
IO.puts capitalized_string
```

Salida:

```
Elixir programming
```

Para casos donde se necesita más control o una lógica de capitalización más compleja, podrías combinar diferentes funciones de String. Por ejemplo, si quieres capitalizar cada palabra en una oración, puedes dividir la oración en palabras, capitalizar cada una y luego unirlas de nuevo:

```elixir
sentence = "elixir is fun"
capitalized_sentence = sentence 
                        |> String.split() 
                        |> Enum.map(&String.capitalize/1) 
                        |> Enum.join(" ")

IO.puts capitalized_sentence
```

Salida:

```
Elixir Is Fun
```

Aunque la biblioteca estándar de Elixir cubre la mayoría de las necesidades, para manipulaciones de texto más matizadas, incluyendo capitalización avanzada de cadenas, podrías explorar librerías de terceros como Cldr para internacionalización, que pueden ofrecer comportamientos de capitalización específicos del idioma.

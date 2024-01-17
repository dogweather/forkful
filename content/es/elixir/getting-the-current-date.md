---
title:                "Obteniendo la fecha actual"
html_title:           "Elixir: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Obtener la fecha actual es un proceso común en la programación, ya que permite a los desarrolladores conocer la fecha y hora del sistema en el que se está ejecutando su código. Esto puede ser útil para registrar la fecha de creación de archivos, hacer cálculos con fechas, o simplemente para fines de registro en una aplicación.

## Cómo hacerlo:

```Elixir
Date.utc_today() 
#=> ~U[2021-10-11]

Date.local_today()
#=> ~D[2021-10-11]

DateTime.utc_now()
#=> ~U[2021-10-11 19:45:21]

DateTime.local_now()
#=> ~U[2021-10-11 16:45:21]
```

## Profundizando:

Obtener la fecha actual puede ser más complejo de lo que parece a simple vista. Antes de los sistemas informáticos, se utilizaban relojes mecánicos y calendarios para registrar la fecha actual. Sin embargo, con la llegada de la computación, se tuvo que encontrar una forma de hacerlo a nivel de software.

Existen varias alternativas para obtener la fecha actual en Elixir, como utilizar la biblioteca de tiempo "Time", o incluso hacer una llamada al sistema operativo utilizando módulos como "Os". Sin embargo, las funciones Date y DateTime proporcionadas por el módulo "Elixir" son fáciles de utilizar y ofrecen una amplia gama de opciones para manipular y formatear fechas y horas.

En cuanto a la implementación, Elixir utiliza el estándar ISO 8601 para representar fechas y horas en su forma predeterminada. Esto permite una fácil conversión a otros formatos y garantiza la compatibilidad con otros sistemas que también utilizan este estándar.

## Ver también:

- Documentación oficial de Elixir: https://hexdocs.pm/elixir/Date.html
- Más información sobre el estándar ISO 8601: https://www.iso.org/iso-8601-date-and-time-format.html
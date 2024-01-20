---
title:                "Convirtiendo una fecha en una cadena de texto"
html_title:           "C++: Convirtiendo una fecha en una cadena de texto"
simple_title:         "Convirtiendo una fecha en una cadena de texto"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Convertir una fecha en una cadena implica la conversión de un formato de fecha numérica o de otro tipo a una cadena de texto legible y entendible. Los programadores lo hacen para facilitar la visualización y manipulación de las fechas.

## Cómo hacerlo:

```Fish Shell
# Primero, vamos a obtener la fecha actual.
set fecha (date)

# Ahora, la convertimos en cadena de texto.
set cadena_fecha (date -u -d $fecha "+%d/%m/%Y %H:%M:%S")

# Mostramos la fecha como cadena de texto.
echo $cadena_fecha
```

La salida de este pequeño script sería algo como:

```Fish Shell
20/05/2022 14:53:35
```

## Análisis avanzado:

(1) **Contexto histórico**: La necesidad de convertir las fechas a cadenas de texto ha existido desde el inicio de la informática. Esto se debía a que las fechas se almacenan de manera eficiente como números, pero los humanos las entendemos mejor como texto.

(2) **Alternativas**: Fish Shell ofrece diferentes maneras para convertir una fecha en una cadena, como el uso de estrings de formato personalizados. Por ejemplo:

```Fish Shell
set fecha_custom (date -u "+%Y-%m-%d")
```

(3) **Detalles de implementación**: El comando 'date' de Fish toma un tiempo (o la fecha/hora actual si no se da ninguno) y formatea ese tiempo de acuerdo a la cadena de formato dada.

## Consulta también:

Echa un vistazo a estos enlaces para aprender más:
- [Variables en Fish](https://fishshell.com/docs/current/lang-vars.html)
- [Tutorial de la línea de comandos de Fish](https://fishshell.com/docs/current/tutorial.html)
---
title:                "Comparando dos fechas"
html_title:           "Bash: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué

Comparar dos fechas en Bash es una práctica común en la programación, ya sea para realizar operaciones matemáticas o para determinar la fecha más reciente o más antigua. En este artículo, aprenderás cómo comparar dos fechas en Bash de forma sencilla y efectiva.

## Cómo hacerlo

Para comparar dos fechas en Bash, primero debes asegurarte de tener las fechas en un formato reconocible por el sistema. Utilizaremos el comando `date` para obtener la fecha actual y el comando `read` para capturar la fecha que ingrese el usuario.

```Bash
# Obtener la fecha actual
fecha_actual=$(date +%Y-%m-%d)

# Capturar la fecha ingresada por el usuario
echo "Ingresa una fecha en formato YYYY-MM-DD:"
read fecha_ingresada
```

Una vez que tenemos ambas fechas, podemos utilizar el operador de comparación `>` (mayor que) o `<` (menor que) para determinar cuál es la fecha más reciente o más antigua.

```Bash
if [[ $fecha_actual > $fecha_ingresada ]]; then
    echo "La fecha actual es más reciente que la fecha ingresada"
else 
    echo "La fecha ingresada es más reciente que la fecha actual"
fi
```

Si quieres comparar fechas en un formato diferente, puedes utilizar el comando `date` para convertir las fechas a un formato específico antes de compararlas. Por ejemplo, si quieres comparar fechas en formato DD-MM-YYYY, puedes utilizar el siguiente código:

```Bash
# Convertir la fecha actual a formato DD-MM-YYYY
fecha_actual_ddmmyyyy=$(date +%d-%m-%Y)

# Convertir la fecha ingresada por el usuario a formato DD-MM-YYYY
fecha_ingresada_ddmmyyyy=$(date -d "$fecha_ingresada" +%d-%m-%Y)

# Comparar las fechas en formato DD-MM-YYYY
if [[ $fecha_actual_ddmmyyyy > $fecha_ingresada_ddmmyyyy ]]; then
    echo "La fecha actual es más reciente que la fecha ingresada"
else 
    echo "La fecha ingresada es más reciente que la fecha actual"
fi
```

## Profundizando

Hay varios factores a tener en cuenta al comparar fechas en Bash. En primer lugar, debes asegurarte de que ambas fechas estén en el mismo formato antes de realizar la comparación. De lo contrario, el resultado puede no ser el esperado.

Además, el operador de comparación `>` y `<` solo funciona con fechas en formato numérico. Si quieres comparar fechas en un formato diferente, como texto, deberás utilizar otros métodos, como la función `diff` o el comando `sort`.

## Ver también

* [Documentación oficial de Bash](https://www.gnu.org/software/bash/)
* [Guía de referencia de Bash](https://www.shellscript.sh/)
* [Cheat sheet de Bash](https://devhints.io/bash)
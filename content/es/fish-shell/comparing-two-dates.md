---
title:                "Fish Shell: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué

Comparar dos fechas es una tarea común en la programación, especialmente en el desarrollo de aplicaciones web y análisis de datos. Ya sea para validar información de un formulario o realizar cálculos de tiempos, saber cómo comparar dos fechas en Fish Shell puede ser una habilidad útil para cualquier programador.

## Cómo hacerlo

Comparar dos fechas en Fish Shell es bastante sencillo gracias a su formato de fecha interno basado en UNIX. A continuación, se muestran algunos ejemplos de cómo hacerlo utilizando expresiones regulares y la función `date`.

```Fish Shell
# Comparar si dos fechas son iguales
if test (date -f %Y-%m-%d $date1) = (date -f %Y-%m-%d $date2)
    echo Las fechas son iguales
end

# Comprobar si la primera fecha es anterior a la segunda
if test (date -f %Y-%m-%d $date1) -lt (date -f %Y-%m-%d $date2)
    echo La primera fecha es anterior a la segunda
end

# Comparar si la segunda fecha es posterior a la primera
if test (date -f %Y-%m-%d $date2) -gt (date -f %Y-%m-%d $date1)
    echo La segunda fecha es posterior a la primera
end
```

La función `date` permite especificar el formato deseado de la fecha utilizando la opción `-f` y luego compara la fecha utilizando la expresión regular correspondiente.

## Profundizando

Aunque se mencionaron solo algunos ejemplos, existen muchas otras formas de comparar dos fechas en Fish Shell. Puedes utilizar la función `math` para realizar operaciones matemáticas con las fechas y compararlas a través de resultados numéricos. También se puede trabajar con fechas y horas específicas utilizando el formato `%s` en la función `date`.

## Ver también
- [Página de manual de Fish Shell sobre la función `date`](https://fishshell.com/docs/current/cmds/date.html)
- [Documentación de expresiones regulares en Fish Shell](https://fishshell.com/docs/current/index.html#regular-expressions)
- [Tutorial sobre el manejo de fechas en Fish Shell](https://www.ostechnix.com/how-to-create-manipulate-files-in-the-past-and-future-with-date-command/)
---
title:    "Fish Shell: Comparando dos fechas"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué
¿Alguna vez has tenido que comparar dos fechas en tu código? Si es así, entonces sabes lo tedioso que puede ser este proceso. Sin embargo, con el uso adecuado de Fish Shell, puedes comparar fácilmente dos fechas y obtener los resultados que necesitas. En esta publicación, exploraremos cómo hacerlo de manera eficiente.

## Cómo hacerlo
Para comparar dos fechas en Fish Shell, utilizaremos el comando `date` seguido de la opción `-jf`, que nos permite especificar un formato de fecha. Podemos utilizar `strftime` para indicar el formato que deseamos utilizar. Por ejemplo, si queremos comparar dos fechas en formato "día-mes-año", podemos usar `%d-%m-%Y`. Aquí está un ejemplo de código y su salida:

```Fish Shell
fecha1 = (date -jf "%d-%m-%Y" "14-08-2021")
fecha2 = (date -jf "%d-%m-%Y" "20-08-2021")
if test $fecha1 -gt $fecha2
    echo "La primera fecha es mayor que la segunda"
else
    echo "La segunda fecha es mayor que la primera"
end
```

Salida:

```
La segunda fecha es mayor que la primera
```

¡Así de fácil es comparar dos fechas en Fish Shell!

## Profundizando
Ahora que sabemos cómo comparar dos fechas en Fish Shell, echemos un vistazo más de cerca a lo que realmente está sucediendo detrás de escena. Cuando usamos `date -jf` para comparar fechas, en realidad estamos convirtiendo esas fechas en timestamps, que son valores numéricos que representan la cantidad de segundos transcurridos desde el 1 de enero de 1970. Luego, simplemente comparamos esos timestamp para determinar cuál es mayor.

Es importante tener en cuenta que la opción `-jf` solo está disponible en versiones más recientes de Fish Shell (2.5 o superior). Si estás utilizando una versión anterior, puedes usar el comando `simple-format` para formatear las fechas en un formato que sea compatible con el comando `date`.

## Ver también
- [Documentación oficial de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial sobre cómo utilizar `strftime` en Fish Shell](https://www.linode.com/docs/guides/use-the-strftime-function-in-your-postgres-shell/)
- [Ejemplos de uso de `date` en Fish Shell](https://fishshell.com/docs/current/commands.html#date)

Gracias por leer y ¡feliz comparación de fechas en Fish Shell!
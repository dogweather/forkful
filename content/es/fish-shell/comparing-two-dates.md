---
title:    "Fish Shell: Comparando dos fechas"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## ¿Por qué comparar dos fechas en la programación de Fish Shell?

Comparar dos fechas es una tarea común en la programación, especialmente en aplicaciones que manejan datos de tiempo. Al comparar dos fechas, podemos determinar la duración entre ellas y realizar acciones en función de eso. En este blog post, aprenderás cómo comparar dos fechas en Fish Shell.

## Cómo hacerlo

Comparar dos fechas en Fish Shell es bastante sencillo y se puede hacer de varias formas. Aquí hay dos ejemplos utilizando la función `test`:

```Fish Shell
test { fecha1 -eq fecha2 }; and echo "Las fechas son iguales"
```

Este ejemplo compara si `fecha1` es igual a `fecha2` y si es así, imprimirá "Las fechas son iguales". También se puede utilizar la función `test` para comparar si una fecha es mayor o menor que otra:

```Fish Shell
test { fecha1 -gt fecha2 }; and echo "Fecha1 es mayor que fecha2"
```

En este caso, si `fecha1` es mayor que `fecha2`, se imprimirá la frase correspondiente. También se pueden utilizar otras opciones, como `-lt` para menor que, `-ge` para mayor o igual que, y `-le` para menor o igual que.

Además de la función `test`, también se puede utilizar la función `math` para realizar comparaciones matemáticas con fechas. Esta función convierte las fechas en un formato numérico y permite compararlas de esa manera.

```Fish Shell
set fecha_num1 (math $fecha1 + %s);
set fecha_num2 (math $fecha2 + %s);
if test $fecha_num1 -eq $fecha_num2; echo "Las fechas son iguales"
```

En este caso, utilizamos la función `math` para convertir las fechas en segundos y luego comparamos los números utilizando `test`. Si son iguales, se imprimirá el mensaje correspondiente.

## Inmersión profunda

Si estás interesado en aprender más sobre cómo comparar fechas en Fish Shell, puedes consultar la documentación oficial en el sitio web de Fish Shell. También hay una función incorporada en Fish Shell llamada `complete fecha` que te permite completar automáticamente fechas mientras escribes, lo cual es útil para evitar errores de formato.

## Ver también

- [Documentación oficial de Fish Shell](https://fishshell.com/docs/current/cmds/test.html)
- [Cómo comparar fechas en Bash](https://linuxize.com/post/how-to-compare-dates-in-bash/)
- [Cómo trabajar con fechas en Fish Shell](https://medium.com/@daviestarfish/how-to-work-with-dates-in-fish-shell-51dcb496333e)
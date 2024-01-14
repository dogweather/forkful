---
title:    "Bash: Comparando dos fechas"
keywords: ["Bash"]
---

{{< edit_this_page >}}

# Por Qué Comparar Dos Fechas en Bash

Si estás aprendiendo a programar en Bash, es posible que te preguntes por qué es importante comparar dos fechas en un programa. La respuesta es simple: al comparar dos fechas, podemos determinar la relación entre ellas y tomar decisiones en nuestro código en consecuencia.

## Cómo Comparar Dos Fechas en Bash
```Bash
# Asignamos dos fechas a variables
primera_fecha="2020-01-01"
segunda_fecha="2021-01-01"

# Comparamos las fechas utilizando el comando "test"
if test "$primera_fecha" -lt "$segunda_fecha"
then
  echo "La primera fecha es anterior a la segunda fecha"
elif test "$primera_fecha" -eq "$segunda_fecha"
then
  echo "Las fechas son iguales"
else
  echo "La segunda fecha es anterior a la primera fecha"
fi
```

En este ejemplo, utilizamos el comando "test" para comparar las dos fechas. La opción "-lt" significa "menor que" y "-eq" significa "igual a". También podemos utilizar la opción "-gt" para "mayor que". Es importante tener en cuenta que debemos utilizar el formato "yyyy-mm-dd" al asignar fechas a variables en Bash.

## Profundizando en la Comparación de Fechas

Cuando comparamos dos fechas en Bash, es importante tener en cuenta que estamos comparando cadenas de texto y no fechas en sí. Por lo tanto, debemos asegurarnos de que las fechas estén en el formato correcto (yyyy-mm-dd) para que la comparación sea precisa. También debemos tener en cuenta el orden en el que escribimos las fechas. Si escribimos "2020-01-01" y "2021-01-01", la primera fecha será considerada como anterior a la segunda fecha. Sin embargo, si escribimos "01-01-2020" y "01-01-2021", la primera fecha será considerada como posterior a la segunda fecha, ya que Bash interpreta las cadenas de izquierda a derecha.

Otra consideración importante al comparar fechas en Bash es que los años, meses y días no necesariamente tienen la misma cantidad de dígitos. Por ejemplo, si escribimos "2020-12-01" y "2021-01-01", la primera fecha será considerada como anterior a la segunda fecha, aunque el mes y el día tienen más dígitos en la primera fecha.

# Ver también
- [Cómo trabajar con fechas en Bash](https://www.cyberciti.biz/faq/unix-linux-bash-date-formatting-previous-day/)
- [Explicación detallada sobre cómo comparar fechas en Bash](https://www.baeldung.com/linux/comparing-dates-bash)
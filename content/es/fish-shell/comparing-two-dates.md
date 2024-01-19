---
title:                "Comparando dos fechas"
html_title:           "C#: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Comparar dos fechas, es decir, determinar cuál de ellas es más temprana o más tardía, es un problema común en programación. Los programadores lo hacen para organizarse, rastrear eventos y tomar decisiones basadas en el tiempo.

## Cómo se hace:

Esto es cómo puedes comparar dos fechas con Fish Shell. Supongamos que tienes dos fechas y quieres saber cuál es la más reciente:

```Fish Shell
set date1 (date -r 1234567890)
set date2 (date -r 9876543210)

if test (date -jf %s "$date1" +%s) -gt (date -jf %s "$date2" +%s)
    echo "La fecha 1 ($date1) es más reciente que la fecha 2 ($date2)."
else
    echo "La fecha 2 ($date2) es más reciente que la fecha 1 ($date1)."
end
```

Suponiendo que "date1" es "13 Feb 2009 23:31:30" y "date2" es "21 Nov 2286 00:01:50", el resultado sería "La fecha 2 (21 Nov 2286 00:01:50) es más reciente que la fecha 1 (13 Feb 2009 23:31:30)."

## Inmersión Profunda

Comparar fechas es un desafío antiguo y generalizado. En los primeros días de la informática, los programadores tenían que hacer todo tipo de trucos para comparar fechas ya que los sistemas operativos y los lenguajes de programación no tenían funciones incorporadas para hacerlo.

Pero hoy en día, hay muchas formas de comparar fechas. Aparte de Fish Shell, puedes usar Python, Java, C# y muchos otros lenguajes de programación para comparar fechas. Incluso puedes usar SQL si estás trabajando con bases de datos.

En Fish Shell, al comparar fechas, en realidad estamos convirtiendo las fechas a segundos (desde el 1 de enero de 1970), ya que es más fácil comparar números que strings. Esto se hace con la función date de Unix que viene incorporada en el Fish Shell.

## Ver También

Para más detalles sobre la programación de fechas y horas en Fish Shell y otros lenguajes de programación, puedes consultar los siguientes enlaces:

1. [Documentación oficial de Fish Shell](https://fishshell.com/docs/current/index.html)
2. [Guía de programación de fechas y horas en Python](https://docs.python.org/3/library/datetime.html)
3. [Guía de programación de fechas y horas en Java](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
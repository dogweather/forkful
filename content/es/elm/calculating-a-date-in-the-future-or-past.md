---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "Elm: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# ¿Qué y por qué?
Calcular una fecha en el futuro o en el pasado es una tarea común en la programación. Esto implica obtener una fecha específica sumando o restando una cantidad de tiempo a otra fecha dada. Los programadores utilizan esta funcionalidad para determinar vencimientos, programar tareas, entre otras aplicaciones.

# Cómo hacerlo:
Para calcular una fecha en Elm, primero debes importar el módulo `Time` en la parte superior de tu código:
```
import Time
```
Para obtener una fecha en el futuro, podemos utilizar la función `add` y proporcionarle un `Time.Span`, que representa la cantidad de tiempo que deseamos sumar a la fecha actual. Por ejemplo, para obtener la fecha dentro de una semana, podemos hacer lo siguiente:
```
Time.add (Time.weeks 1) Time.now
```
Esto nos devolverá una `Time.Posix` que representa la nueva fecha en el futuro. Similarmente, si queremos obtener una fecha en el pasado, podemos utilizar la función `subtract` y proporcionarle un `Time.Span` negativo. Por ejemplo, para obtener la fecha hace dos días, podemos hacer lo siguiente:
```
Time.subtract (Time.days -2) Time.now
```

# Profundizando:
El cálculo de fechas ha sido una tarea común en la programación desde hace mucho tiempo. Antes de la introducción de bibliotecas y funciones específicas para manejar fechas, los programadores tenían que escribir su propia lógica para calcular fechas en diferentes lenguajes de programación. En Elm, podemos utilizar el módulo `Time` que nos proporciona funciones y tipos de datos específicos para manejar fechas y tiempos.

Otra alternativa para calcular fechas es utilizando bibliotecas externas, como `date-extra`, que proporciona una funcionalidad similar al módulo `Time` pero con algunas características adicionales.

En cuanto a la implementación, el módulo `Time` utiliza objetos `Time.Posix` que representan los momentos en el tiempo. Estos objetos se basan en el [Tiempo Unix](https://es.wikipedia.org/wiki/Tiempo_Unix), que es una medida del tiempo que se ha utilizado ampliamente en sistemas informáticos.

# Ver también:
- Documentación del módulo `Time` en la [página oficial de Elm](https://package.elm-lang.org/packages/elm/time/latest/).
- Puedes encontrar más información sobre el [Tiempo Unix](https://www.unixtimestamp.com/) en línea.
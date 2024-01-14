---
title:    "Ruby: Convirtiendo una fecha en una cadena de caracteres"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías aprender a convertir una fecha en un string en Ruby?

Aprender a convertir una fecha en un string en Ruby puede ser muy útil para trabajar con fechas en tus programas. Te permitirá mostrar la fecha en un formato específico, manipularla y realizar cálculos con ella de una manera más eficiente.

## Cómo hacerlo

Para convertir una fecha en un string en Ruby, podemos utilizar el método `strftime` que se encuentra en la clase `Time`. Este método nos permite especificar el formato en el que queremos que se muestre la fecha. Aquí hay un ejemplo de cómo puedes utilizarlo:

```Ruby
# Convertir una fecha a un string en formato DD/MM/YYYY
Time.now.strftime("%d/%m/%Y")
# Output: "22/10/2021"

# Convertir una fecha a un string en formato DD de mes de YYYY
Time.now.strftime("%d de %B de %Y")
# Output: "22 de Octubre de 2021"
```

Como puedes ver, el formato que le proporcionamos a `strftime` es una combinación de letras y símbolos que representan diferentes partes de la fecha. Algunos de los más comunes son:

- `%d`: día del mes, con ceros iniciales
- `%m`: mes del año, con ceros iniciales
- `%Y`: año con cuatro dígitos
- `%B`: nombre completo del mes
- `%b`: abreviación del nombre del mes
- `%A`: nombre completo del día de la semana
- `%a`: abreviación del nombre del día de la semana

Puedes encontrar una lista completa de los formatos disponibles en la documentación de Ruby.

## Profundizando

Si quieres profundizar en cómo se realiza la conversión de una fecha a un string, es importante entender que las fechas en Ruby son representadas internamente como segundos desde la medianoche del 1 de enero de 1970 en formato UTC. Por lo tanto, la conversión a un string en realidad es una conversión de un número entero a una cadena de texto.

Además, también es importante tener en cuenta que el método `strftime` es sólo un wrapper alrededor de la función estándar `strftime` de C, por lo que su comportamiento dependerá de la implementación de Ruby que estés utilizando.

## Ver también

- [Documentación de Ruby sobre el método `strftime`](https://ruby-doc.org/core/Time.html#method-i-strftime)
- [Lista completa de los formatos disponibles en `strftime`](https://apidock.com/ruby/DateTime/strftime)
- [Cómo trabajar con fechas en Ruby](https://www.rubyguides.com/2016/09/ruby-datetime/)
---
title:    "C#: Convirtiendo una fecha en una cadena"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Por qué

Convertingir una fecha en una cadena de texto es una habilidad importante en programación ya que permite mostrar información de manera legible y entendible para los usuarios. Además, en algunos casos las bases de datos y servidores requieren fechas en formato de cadena de texto para almacenar y procesar la información correctamente.

## Cómo hacerlo

```C#
//Usar el método ToString() para convertir una fecha en una cadena de texto
DateTime fecha = new DateTime(2020, 05, 01); // 1 de mayo del 2020
string fechaString = fecha.ToString(); // "5/1/2020 12:00:00 AM"

//Usar el método ToString(format) para especificar el formato deseado
fechaString = fecha.ToString("dd/MM/yyyy"); // "01/05/2020"
fechaString = fecha.ToString("MMM dd, yyyy"); // "May 01, 2020"

//Usar el método ToString(format, culture) para especificar el idioma según la cultura
fechaString = fecha.ToString("d", new CultureInfo("es-ES")); // "01/05/2020"
fechaString = fecha.ToString("MMMM dd, yyyy", new CultureInfo("es-ES")); // "mayo 01, 2020"

//Usar un formato personalizado
fechaString = fecha.ToString("dddd, d 'de' MMMM 'de' yyyy", new CultureInfo("es-ES")); // "viernes, 1 de mayo de 2020"

```

La clase ```DateTime``` de C# proporciona varios métodos para convertir fechas en cadenas de texto con diferentes formatos y personalizaciones. Es importante tener en cuenta que la cadena de texto resultante dependerá de la cultura y el idioma utilizados.

## Deep Dive

El método ToString() de la clase ```DateTime``` acepta un parámetro opcional de formato que nos permite especificar cómo queremos que se muestre la fecha en la cadena de texto resultante. Algunos de los formatos más comunes son:

- ```d``` para mostrar la fecha en formato corto, como "MM/dd/yyyy"
- ```D``` para mostrar la fecha en formato largo, como "MMMM dd, yyyy"
- ```t``` para mostrar la hora en formato corto, como "hh:mm tt"
- ```T``` para mostrar la hora en formato largo, como "hh:mm:ss tt"
- ```f``` para mostrar la fecha y hora en formato largo, como "MMMM dd, yyyy hh:mm tt"
- ```g``` para mostrar la fecha y hora en formato corto, como "MM/dd/yyyy hh:mm tt"

Además, el método ToString() permite especificar un proveedor de formato de fecha y hora personalizado, lo que nos permite tener un mayor control sobre el formato de la cadena resultante. También es importante mencionar que los caracteres utilizados en el formato dependen de la cultura especificada, por lo que se recomienda tener en cuenta estos aspectos al trabajar con fechas y cadenas de texto en diferentes idiomas.

## Ver también

- Documentación oficial de Microsoft sobre el método DateTime.ToString(): https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring?view=netcore-3.1
- Ejemplos de formato de fecha en diferentes idiomas: https://www.c-sharpcorner.com/blogs/date-and-time-format-in-c-sharp-programming1
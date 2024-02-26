---
title:                "Interpolando una cadena de texto"
date:                  2024-02-25T17:07:00.069426-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-02-25, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Qué y por qué
La interpolación de cadenas en C# te permite crear una nueva cadena al incluir expresiones dentro de un literal de cadena, lo que facilita el formateo y la concatenación de cadenas. Los programadores utilizan esta característica para mejorar la legibilidad y mantenibilidad del código, especialmente cuando se trata de contenido dinámico de cadenas.

## Cómo hacerlo:
En C#, la interpolación de cadenas se indica con un signo de dólar (`$`) seguido de un literal de cadena. Los nombres de las variables o las expresiones se encierran entre llaves (`{}`).

```csharp
string name = "Jane";
int age = 28;
string interpolatedString = $"Hola, {name}! Tienes {age} años.";
Console.WriteLine(interpolatedString);
// Salida: Hello, Jane! Tienes 28 años.
```

En un ejemplo más complejo, puedes realizar operaciones o llamar a métodos dentro de las llaves:

```csharp
double price = 19.99;
int quantity = 3;
string orderDetail = $"Precio total: {price * quantity:C2}";
Console.WriteLine(orderDetail);
// Salida: Precio total: $59.97
```
El especificador de formato `:C2` dentro de las llaves formatea el número como una moneda con dos decimales.

Para escenarios que requieran un formato más avanzado o localización, podrías considerar usar el método `string.Format` o librerías como Humanizer. Humanizer puede manipular y mostrar cadenas, fechas, horas, intervalos de tiempo, números y cantidades de una forma más legible para el ser humano. A continuación, se muestra un ejemplo del uso de Humanizer para manipulaciones complejas de cadenas. Note que Humanizer no es parte de la biblioteca estándar de .NET y requiere instalar el paquete NuGet `Humanizer`.

Primero, instala Humanizer a través de NuGet:

```
Install-Package Humanizer
```

Luego, puedes usarlo de la siguiente manera:

```csharp
using Humanizer;

int dayDifference = 5;
string humanized = $"El evento fue hace {dayDifference} días.".Humanize();
Console.WriteLine(humanized);
// Dependiendo de la configuración y la cultura, una posible salida: El evento fue hace 5 días.
```

Este ejemplo demuestra el uso básico. Humanizer soporta un amplio rango de funcionalidades que se pueden aplicar a cadenas, fechas, números y más, haciendo tus aplicaciones más accesibles e intuitivas.

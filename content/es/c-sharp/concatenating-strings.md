---
title:    "C#: Concatenando cadenas"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Por qué:

La concatenación de cadenas es una técnica esencial en la programación de C# ya que nos permite combinar múltiples cadenas de texto en una sola. Esto puede ser útil en situaciones como la creación de mensajes personalizados, la manipulación de datos, o la construcción de rutas de archivo.

## Cómo hacerlo:

 En C#, concatenar cadenas es bastante sencillo. Primero, necesitamos definir dos o más cadenas que queremos unir. Por ejemplo:

```C#
string saludo = "¡Hola";
string nombre = "Carlos!";
```
Luego, podemos usar el operador de suma (+) para unir las cadenas:

```C#
string fraseCompleta = saludo + " " + nombre;
```
En este caso, la cadena resultante será "¡Hola Carlos!". También podemos incluir variables, números o incluso expresiones dentro de la concatenación. Aquí hay un ejemplo con una expresión:

```C#
int edad = 25;
string mensaje = "Tengo " + edad + " años.";
```
La cadena resultante será "Tengo 25 años.". Ten en cuenta que cuando usamos expresiones en una concatenación, deben estar dentro de paréntesis para asegurar que la operación se realice correctamente.

## Profundizando:

 Además de utilizar el operador de suma, también podemos usar la clase `StringBuilder` para construir cadenas. Esta clase ofrece métodos más eficientes para agregar y manipular cadenas en comparación con el operador de suma.

También es importante destacar que en C#, las cadenas son objetos inmutables, lo que significa que no pueden ser modificadas una vez creadas. Por lo tanto, cada vez que concatenamos cadenas, en realidad estamos creando una nueva cadena en lugar de modificar la existente.

## Ver también:

- [Documentación oficial de Microsoft sobre la concatenación de cadenas en C#](https://docs.microsoft.com/es-es/dotnet/csharp/programming-guide/strings/#concat)
- [Tutorial de concatenación de cadenas en C# en español](https://www.tutorialspoint.com/es/csharp/string_concatenation.htm)
- [Otra forma de unir cadenas en C# usando el método `string.Format()`](https://docs.microsoft.com/es-es/dotnet/standard/base-types/composite-formatting?redirectedfrom=MSDN&view=netframework-4.7.2)
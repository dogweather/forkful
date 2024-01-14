---
title:                "C++: Imprimiendo salida de depuración"
programming_language: "C++"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

A veces, cuando escribimos programas en C++, podemos enfrentarnos a errores o problemas que no podemos solucionar fácilmente. En estos casos, puede ser de gran ayuda imprimir información de depuración en la consola para comprender mejor lo que está sucediendo en nuestro código. Como programadores, es importante saber cómo imprimir información de depuración en C++ para facilitar la solución de problemas en nuestro código.

## Cómo hacerlo

Imprimir información de depuración en C++ es muy fácil y solo requiere una línea de código. Utilizando la función `cout` de la biblioteca estándar, podemos imprimir cualquier variable o mensaje en la consola. Aquí hay un ejemplo de cómo imprimir el valor de una variable entera llamada `numero`:

```C++
cout << "El número es: " << numero << endl;
```

El uso del operador `<<` nos permite imprimir varias variables o mensajes juntos. Además, el uso del manipulador de flujo `endl` nos permite enviar un carácter de nueva línea al final del mensaje impreso.

Aquí hay un ejemplo de ejecución del código anterior:

```
El número es: 10
```

Es importante tener en cuenta que al imprimir diferentes tipos de datos, debemos utilizar el operador correcto. Para variables de tipo `string` o `char` debemos utilizar el operador `<<`, mientras que para variables de tipo `integer` o `double` debemos utilizar el operador `+`.

## Deep Dive

Además de imprimir variables o mensajes en la consola, también podemos utilizar la información de depuración para verificar si nuestro código está funcionando correctamente. En lugar de imprimir todos los resultados en la consola, podemos utilizar una declaración `if` para imprimir solo cuando se cumple una determinada condición.

Por ejemplo, supongamos que tenemos un programa que calcula el área de un triángulo. Podemos utilizar una declaración de impresión de depuración para asegurarnos de que la fórmula que estamos utilizando es correcta:

```C++
if (altura > base) {
    cout << "La altura es mayor que la base, por lo que el resultado puede ser incorrecto." << endl;
}
```

De esta forma, podemos detectar y corregir errores en nuestro código a medida que avanzamos en el desarrollo.

## Ver también

- [Aprende C++](https://www.aprendecpp.com/)
- [Documentación de C++](https://devdocs.io/cpp/)
- [Solución de problemas en C++](https://www.codementor.io/@martinagudo/troubleshooting-tips-for-c-BBFj9sKLz)
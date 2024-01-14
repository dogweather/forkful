---
title:    "C++: Concatenando cadenas"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Por qué

Concatenar cadenas de texto es una técnica esencial en la programación de C++. Permite combinar varias cadenas para formar una sola, lo que resulta muy útil en la creación de mensajes personalizados, generación de nombres y mucho más.

## Cómo hacerlo

La concatenación de cadenas de texto se puede realizar de diferentes maneras en C++. Una forma común es utilizando el operador de suma (+), que nos permite unir dos cadenas tal como lo haríamos con números. Veamos un ejemplo:

```C++
#include <iostream>

using namespace std;

int main() {

    string nombre = "Juan";
    string apellido = "Pérez";

    string nombre_completo = nombre + " " + apellido;

    cout << nombre_completo << endl;

    return 0;
}
```

**Salida:** Juan Pérez

En el ejemplo, declaramos dos variables de tipo string y luego las concatenamos utilizando el operador de suma (+). También podemos utilizar la función `append()` para agregar una cadena al final de otra. Veamos otro ejemplo:

```C++
#include <iostream>

using namespace std;

int main() {

    string pais = "España";
    string mensaje = "¡Hola! ";

    mensaje.append(pais);

    cout << mensaje << endl;

    return 0;
}
```

**Salida:** ¡Hola! España

También es posible concatenar cadenas utilizando la función `concat()` de la librería `string`, que nos permite unir varias cadenas en una sola llamada. Veamos un ejemplo más:

```C++
#include <iostream>
#include <string> // Requerido para la función concat()

using namespace std;

int main() {

    string dia = "lunes";
    string fecha = "25 de enero de 2021";
    string mensaje = "Hoy es ";

    mensaje = mensaje.concat(dia, " ", fecha);

    cout << mensaje << endl;

    return 0;
}
```

**Salida:** Hoy es lunes 25 de enero de 2021

## Profundizando

La concatenación de cadenas de texto es una técnica fundamental en la programación de C++, pero es importante tener en cuenta algunos aspectos para evitar errores y obtener un código más eficiente.

Es importante recordar que las cadenas de texto en C++ se almacenan como arrays de caracteres, por lo que al concatenar, se deben tener en cuenta los límites del array para evitar desbordamientos de búfer. Además, si se están concatenando grandes cantidades de cadenas, es aconsejable utilizar la clase `stringstream` que ofrece un mejor rendimiento.

## Ver también

- [Documentación de concatenación de cadenas en C++](https://www.cplusplus.com/reference/string/string/append/)
- [Tutorial de C++: Aprende a concatenar cadenas de texto](https://www.programiz.com/cpp-programming/string-concatenation)
---
title:    "C++: Escribiendo en el error estándar"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

#¿Por qué escribir en estándar error en C++?

La salida estándar (stdout) y la salida de error (stderr) son dos formas en las que un programa puede mostrar información al usuario. La principal diferencia es que la salida estándar se muestra en la consola del usuario, mientras que la salida de error se guarda en un archivo o se muestra en la consola de errores. Escribir en estándar error puede ser útil cuando quieres mostrar información importante sobre un error o cuando quieres guardar información de depuración en un archivo separado.

##Cómo escribir en estándar error en C++

En C++, existe una función llamada "std::cerr" que permite escribir en la salida de error. Para usar esta función, debes incluir la librería <iostream> y luego usarla de la siguiente manera:

```C++
#include <iostream>

int main() {
    std::cerr << "Este es un mensaje de error" << std::endl;
    return 0;
}
```

El código anterior mostrará el mensaje "Este es un mensaje de error" en la consola de errores. Nota que se utiliza el operador "<<" para insertar el mensaje en la función "std::cerr" y se utiliza "std::endl" para indicar el final de la línea.

También puedes usar esta función para mostrar valores de variables mientras estás depurando tu código. Por ejemplo:

```C++
int main() {
    int a = 5;
    std::cerr << "El valor de a es: " << a << std::endl;
    return 0;
}
```

Este código mostrará "El valor de a es: 5" en la consola de errores.

##Deep Dive: Más información sobre escribir en estándar error

Una de las ventajas de escribir en estándar error es que puedes guardar la salida de error en un archivo y revisarla más tarde para encontrar errores en tu código. Para hacer esto en C++, simplemente redirige la salida de errores a un archivo en lugar de mostrarla en la consola. Puedes hacerlo usando el símbolo ">" seguido del nombre del archivo. Por ejemplo:

```
./mi_programa > errores.txt
```

Esto guardará todos los mensajes de error en un archivo llamado "errores.txt".

También puedes usar esta función para mostrar información importante al usuario en caso de un error inesperado. Puedes personalizar tus mensajes de error para que sean claros y útiles para los usuarios.

##Ver también

Para obtener más información sobre la salida estándar y la salida de error en C++, puedes consultar los siguientes recursos:

- https://www.programiz.com/cpp-programming/library-function/cstdio/stderr
- https://www.geeksforgeeks.org/stdcerr-vs-stderr-cpp/
- https://en.cppreference.com/w/cpp/io/cerr
- https://es.cppreference.com/w/cpp/language/io_error
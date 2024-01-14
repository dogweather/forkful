---
title:    "C++: Escribiendo en error estándar"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Por qué: 

Escribir a la "salida de error estándar" puede ser una técnica útil en la programación de C++ para identificar y depurar errores en el código. Con este método, puedes imprimir mensajes específicos en la consola para encontrar y corregir fallas en tu programa.

## Cómo hacerlo:

Para escribir a la salida de error estándar, primero necesitas incluir la librería "iostream". Luego, usa el objeto "cerr" de la clase "ostream" para imprimir tus mensajes de error.

```C++
#include <iostream>

using namespace std;

int main() {
    double divisor = 0;
    double result = 10 / divisor; // This will cause an error
    
    // Print error message to standard error
    cerr << "Error: Cannot divide by zero!" << endl;
    
    return 0;
}
```

La salida de este código sería: 
```
Error: Cannot divide by zero!
```

## Profundizando:

Ahora que sabes cómo escribir a la salida de error estándar, es importante entender que esta técnica puede ser útil para depurar cuando tienes un gran volumen de salida de la consola. Al imprimir mensajes de error en lugar de utilizar la salida estándar, puedes filtrar fácilmente la información relevante para encontrar y resolver problemas en tu código.

## Ver también:

- [Cómo usar la salida de error estándar en C++](https://www.cplusplus.com/reference/iostream/cerr/)
- [Depuración de código en C++](https://docs.microsoft.com/es-es/cpp/debugging/debugging-cpp-code?view=vs-2019)
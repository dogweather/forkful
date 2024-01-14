---
title:    "C++: Capitalizando una cadena"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

La capitalización de una cadena de texto es una tarea común en la programación, especialmente cuando se trabaja con datos de entrada. Al capitalizar una cadena, se puede garantizar que la primera letra de cada palabra esté en mayúscula, lo que facilita la lectura y el procesamiento de la información.

## Cómo hacerlo

La manera más sencilla de capitalizar una cadena de texto en C++ es utilizando la función `toupper()` incluida en la librería `<cctype>`. Esta función toma un carácter como parámetro y devuelve su versión mayúscula. Combinando esta función con un ciclo `for` y un condicional `if`, podemos capitalizar cada letra de la cadena. Veamos un ejemplo:

```C++
#include <iostream>
#include <cstring>
#include <cctype>

using namespace std;

int main(){

    //Definir una cadena de texto
    char cadena[] = "hola mundo";
    //Obtener la longitud de la cadena
    int longitud = strlen(cadena);

    //Ciclo para recorrer cada letra de la cadena
    for(int i = 0; i < longitud; i++){
        //Verificar si es un espacio en blanco
        if(isspace(cadena[i])){
            //Capitalizar la letra siguiente
            cadena[i+1] = toupper(cadena[i+1]);
        }
        //Capitalizar la primera letra de la cadena
        if(i == 0){
            cadena[i] = toupper(cadena[i]);
        }
    }

    //Mostrar la cadena capitalizada
    cout << cadena << endl;

    return 0;
}

```

**Salida:**

```
Hola Mundo
```

## Profundizando

Una forma más eficiente de capitalizar una cadena de texto en C++ es utilizando la función `std::transform()` de la librería `<algorithm>`. Esta función toma tres parámetros: el inicio de la cadena, el final de la cadena y una función o lambda que especifica cómo se va a transformar cada carácter. Veamos un ejemplo:

```C++

#include <iostream>
#include <cstring>
#include <cctype>
#include <algorithm>

using namespace std;

int main(){

    //Definir una cadena de texto
    string cadena = "hola mundo";
    //Utilizar std::transform() para aplicar toupper() a cada carácter
    transform(cadena.begin(), cadena.end(), cadena.begin(),::toupper);

    //Mostrar la cadena capitalizada
    cout << cadena << endl;

    return 0;
}

```

**Salida:**

```
HOLA MUNDO
```

Con este enfoque, no es necesario recorrer la cadena ni utilizar estructuras de control, lo que hace el código más eficiente y fácil de leer.

## Ver también

- [Documentación de la función toupper() en C++](https://www.cplusplus.com/reference/cctype/toupper/)
- [Documentación de la función transform() en C++](https://www.cplusplus.com/reference/algorithm/transform/)
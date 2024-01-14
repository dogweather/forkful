---
title:    "C++: Escritura de un archivo de texto"
keywords: ["C++"]
---

{{< edit_this_page >}}

##Por qué

Escribir un archivo de texto es una habilidad fundamental que todo programador debe tener. Permite guardar y compartir información de manera sencilla y organizada, siendo una herramienta útil para almacenar datos importantes y crear registros en programas.

##Cómo hacerlo

Para escribir un archivo de texto en C++, se utilizan las librerías estándar `fstream` y `iostream`. Aquí mostramos un ejemplo sencillo de cómo escribir un mensaje en un archivo de texto llamado "mensaje.txt":

```
#include <iostream>
#include <fstream>

using namespace std;

int main() {
    // Crear objeto ofstream para manejar el archivo de escritura
    ofstream archivo("mensaje.txt");

    // Verificar si el archivo fue creado correctamente
    if (!archivo) {
        cerr << "No se pudo crear el archivo" << endl;
        return 1;
    }

    // Escribir el mensaje en el archivo
    archivo << "¡Hola! Este es un mensaje de prueba." << endl;
    archivo << "Espero que estés disfrutando de tu aprendizaje en C++." << endl;

    // Cerrar el archivo
    archivo.close();

    // Mensaje de éxito
    cout << "Se ha escrito el mensaje en el archivo correctamente" << endl;

    return 0;
}
```

El código anterior utiliza el operador `<<` para enviar el texto al archivo. El endl al final de cada línea sirve para insertar un salto de línea. Si se omite, el texto se escribirá en una sola línea.

El resultado en el archivo "mensaje.txt" será:

```
¡Hola! Este es un mensaje de prueba.
Espero que estés disfrutando de tu aprendizaje en C++.
```

##Profundizando

Ahora que ya sabes cómo escribir un archivo de texto en C++, es importante entender cómo funciona. En esencia, el código crea un objeto tipo `ofstream`, que se encarga de abrir el archivo y escribir los datos. Si el archivo no existe, se crea automáticamente. Sin embargo, si el archivo ya existe, el contenido anterior se sobrescribirá.

Para escribir en un lugar específico del archivo, se usa el método `seekp()` seguido de un desplazamiento en bytes desde el principio del archivo. También se puede utilizar el método `seekg()` para leer desde un punto específico del archivo.

Ten en cuenta que es necesario cerrar el archivo con el método `close()` para asegurarse de que los datos se guardan correctamente.

##Véase también

- [Librería estándar C++](https://es.cppreference.com/w/c)
- [Tutorial de archivos en C++](https://www.learn-c.org/en/Files)
- [Manipulación de archivos en C++](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
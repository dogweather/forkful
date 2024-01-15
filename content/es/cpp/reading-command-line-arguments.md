---
title:                "Leyendo argumentos de línea de comandos"
html_title:           "C++: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué leer argumentos de línea de comando

Si estás trabajando en un proyecto de programación en C++, es posible que en algún momento necesites pasar información al programa desde la línea de comando. Para ello, es importante saber cómo leer argumentos de línea de comando correctamente. ¡Sigue leyendo para aprender cómo hacerlo!

## Cómo hacerlo

Para leer argumentos de línea de comando en C++, utilizamos la función `main()`, que es donde comienza la ejecución del programa. Esta función toma dos parámetros, `argc` y `argv`, que representan el número de argumentos y un array de cadenas respectivamente.

Por ejemplo, si tenemos un programa que calcula el área de un rectángulo, podemos pasar la base y la altura desde la línea de comando de la siguiente manera:

```C++
int main(int argc, char *argv[]) {
    // verificamos que se hayan pasado los dos argumentos esperados
    if (argc != 3) {
        cout << "Se esperaban dos argumentos: la base y la altura." << endl;
        return 1; // código de error
    }

    // convertimos las cadenas de caracteres a números
    float base = atof(argv[1]);
    float altura = atof(argv[2]);

    // calculamos y mostramos el área del rectángulo
    float area = base * altura;
    cout << "El área del rectángulo es: " << area << endl;
    return 0; // código de éxito
}
```

Si ejecutamos este programa desde la línea de comando con los argumentos adecuados, obtendremos el siguiente resultado:

```
$ ./area_rectangulo 5.5 3
El área del rectángulo es: 16.5
```

## Profundizando

Cuando utilizamos la función `main()`, el primer argumento (`argv[0]`) siempre es el nombre del programa en sí. A partir del segundo argumento, podemos pasar la información que necesitemos al programa.

Además, es importante mencionar que los argumentos de línea de comando siempre son tratados como cadenas de caracteres, por lo que si necesitamos utilizarlos como otros tipos de datos (como en el ejemplo anterior), tendremos que realizar conversiones.

## Ver también

- [Tutorial de programación C++ de Codecademy](https://www.codecademy.com/learn/learn-c-plus-plus)
- [Documentación de la función `main()` en cplusplus.com](http://www.cplusplus.com/reference/cstdlib/main/)
- [Artículo sobre argumentos de línea de comando en cplusplus.com](http://www.cplusplus.com/articles/DEN36Up4/)
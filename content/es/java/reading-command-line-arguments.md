---
title:    "Java: Leyendo argumentos de línea de comandos"
keywords: ["Java"]
---

{{< edit_this_page >}}

##¿Por qué leer los argumentos de la línea de comando en Java?

Los argumentos de la línea de comando son una forma eficaz de proporcionar información a un programa Java antes de su ejecución. En lugar de especificar constantemente los mismos valores en el código, se pueden pasar como argumentos externos, lo que permite una mayor flexibilidad y facilita el uso de diferentes valores sin tener que modificar el código.

##Cómo leer los argumentos de la línea de comando en Java

Para leer los argumentos de la línea de comando en Java, se deben seguir estos pasos:

1. Declarar variables para almacenar los argumentos en el método `main()`.
2. Utilizar el parámetro `args` en el método `main()` como un array de strings para almacenar los argumentos.
3. Utilizar un loop para recorrer los argumentos y utilizarlos según sea necesario.

Veamos un ejemplo de código que muestra cómo leer los argumentos de la línea de comando y mostrarlos en la consola:

```Java
public class CommandLineArgs {
    public static void main(String[] args) {
        // Declarar variables para almacenar los argumentos
        String nombre = "";
        int edad = 0;
        
        // Utilizar un loop para recorrer los argumentos
        for (int i = 0; i < args.length; i++) {
            // Utilizar el argumento adecuado según su posición
            if (i == 0) {
                nombre = args[i];
            } else {
                // Convertir el argumento en número antes de asignarlo a la variable
                edad = Integer.parseInt(args[i]);
            }
        }
        
        // Mostrar los argumentos en la consola
        System.out.println("Hola " + nombre + ", tu edad es: " + edad);
    }
}
```

Si ejecutamos este programa con los argumentos "Juan" y "25":

`java CommandLineArgs Juan 25`

Obtendremos el siguiente resultado en la consola:

`Hola Juan, tu edad es: 25`

##Profundizando en la lectura de argumentos de la línea de comando en Java

Existen diferentes formas de leer y utilizar los argumentos de la línea de comando en Java. Por ejemplo, se pueden utilizar librerías como Apache Commons CLI o usar métodos específicos de la clase `System` como `getProperties()` o `getenv()`. También se puede validar los argumentos ingresados por el usuario para evitar posibles errores en la ejecución del programa.

Es importante tener en cuenta que los argumentos de la línea de comando se pasan como strings, por lo que se deben convertir a otros tipos de datos si es necesario. También es posible pasar argumentos con opciones o flags, lo que permite realizar diferentes acciones según los argumentos ingresados.

##Ver también

- [Documentación de Oracle sobre el uso de argumentos de la línea de comando en Java](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Tutorial de Baeldung sobre el uso de argumentos de la línea de comando en Java](https://www.baeldung.com/java-command-line-arguments)
- [Uso de Apache Commons CLI en Java](https://commons.apache.org/proper/commons-cli/usage.html)
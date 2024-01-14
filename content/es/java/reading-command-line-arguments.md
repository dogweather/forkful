---
title:                "Java: Leyendo argumentos de línea de comandos"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué leer argumentos de línea de comando en Java

Si eres un programador Java, es muy probable que hayas escuchado sobre los argumentos de línea de comando. Pero, ¿sabes por qué son importantes y cómo puedes utilizarlos en tus programas? A continuación, te explicaremos todo lo que necesitas saber sobre este tema.

## Cómo leer argumentos de línea de comando en Java

Leer argumentos de línea de comando en Java es muy sencillo. Solo necesitas seguir los siguientes pasos:

1. Define el método `main()` en tu clase Java.
2. Agrega un parámetro de tipo `String[] args` al método `main()`.
3. Utiliza un bucle `for` para recorrer todos los argumentos introducidos en la línea de comando.
4. Usa el método `System.out.println()` para imprimir cada argumento en la consola.

```Java
public class Main {
   public static void main(String[] args) {
      for(int i=0; i<args.length; i++) {
         System.out.println(args[i]);
      }
   }
}
```

Si ejecutas este código en la línea de comando con argumentos como `java Main argumento1 argumento2`, obtendrás la siguiente salida:

```
argumento1
argumento2
```

## Profundizando en la lectura de argumentos de línea de comando en Java

Ahora que sabes cómo leer los argumentos de línea de comando en Java, te preguntarás ¿por qué necesitamos hacer esto? Bueno, los argumentos de línea de comando nos permiten pasar información a nuestro programa al momento de ejecutarlo. Esto es muy útil si queremos personalizar ciertos aspectos de nuestro programa sin tener que modificar su código fuente.

Además, también podemos utilizar argumentos de línea de comando para crear aplicaciones interactivas en la consola, donde el usuario pueda introducir datos a través de los argumentos en lugar de tener que escribirlos manualmente.

También es importante mencionar que los argumentos de línea de comando se pueden utilizar junto con las variables de entorno del sistema para mejorar la seguridad de nuestras aplicaciones.

## Ver también

- [Cómo utilizar variables de entorno en Java](https://link1.com)
- [Ejemplos de aplicaciones interactivas en consola](https://link2.com)
- [Documentación oficial sobre los argumentos de línea de comando en Java](https://link3.com)
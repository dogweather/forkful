---
title:    "Java: Comprobando si existe un directorio"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

Comprobar si un directorio existe puede ser un paso importante en la programación, ya que permite verificar si se tiene acceso a un determinado directorio antes de realizar cualquier operación en él.

## Cómo hacerlo

Para comprobar si un directorio existe en Java, podemos utilizar la clase `File` y su método `exists()`. Este método devuelve un valor `true` en caso de que el directorio exista, o `false` en caso contrario.

````Java
File directorio = new File("ruta/a/directorio");
if (directorio.exists()) {
  System.out.println("El directorio existe.");
} else {
  System.out.println("El directorio no existe.");
}
````

Además, también podemos utilizar el método `isDirectory()` para verificar si se trata de un directorio y no de un archivo.

````Java
File directorio = new File("ruta/a/directorio");
if (directorio.isDirectory()) {
  System.out.println("La ruta apunta a un directorio.");
} else {
  System.out.println("La ruta no apunta a un directorio.");
}
````

## Profundizando

El método `exists()` de la clase `File` utiliza el sistema de archivos subyacente para comprobar si un directorio existe. Por lo tanto, es importante tener en cuenta que este método puede no ser 100% preciso, ya que puede haber cambios en el sistema de archivos entre el momento de la comprobación y el momento de realizar una operación en el directorio.

También es importante tener en cuenta la seguridad al comprobar si un directorio existe. Por ejemplo, un usuario malintencionado podría crear un directorio con un nombre similar al que estamos buscando, para así engañar a nuestro programa.

## Ver también

- [Documentación de la clase File en Java](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Tutorial sobre el uso de la clase File en Java](https://www.baeldung.com/java-file)
- [Más información sobre seguridad en el acceso a archivos y directorios en Java](https://docs.oracle.com/javase/tutorial/essential/io/security.html)
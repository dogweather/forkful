---
title:    "Gleam: Comenzando un nuevo proyecto"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por qué

Empezar un nuevo proyecto siempre es emocionante y puede ser una gran oportunidad para aprender algo nuevo y potencialmente crear algo increíble. Además, puede ser una excelente manera de mejorar tus habilidades de programación en Gleam.

## Cómo hacerlo

Para comenzar un nuevo proyecto en Gleam, primero debes asegurarte de tener el compilador de Gleam instalado en tu sistema. Una vez instalado, puedes crear un nuevo directorio para tu proyecto y un archivo `gleam.toml` en el que especificarás la configuración de tu proyecto.

Después de eso, puedes iniciar un nuevo módulo con el siguiente comando:

```Gleam
gleam new module hello_world
```

Esto creará un archivo llamado `hello_world.gleam` en tu directorio de proyecto. Puedes abrir este archivo en tu editor de texto y comenzar a escribir tu código. Por ejemplo, puedes imprimir "¡Hola, mundo!" en la consola con este código:

```Gleam
import gleam/io

pub fn main() {
    let message = "Hola, mundo!"
    io.println(message)
}
```

Después de guardar el archivo, puedes compilar tu proyecto con el siguiente comando:

```Gleam
gleam build src/hello_world.gleam
```

Si todo salió bien, deberías ver un archivo `.beam` en tu directorio de compilación. ¡Felicidades, acabas de crear tu primer proyecto en Gleam!

## Profundizando

Si quieres profundizar más en cómo comenzar un nuevo proyecto en Gleam, puedes leer la documentación oficial de Gleam y jugar con algunos otros ejemplos. También es útil seguir a la comunidad de Gleam en línea y unirse a canales de chat como Gitter o Discord para obtener ayuda y consejos de otros programadores en Gleam.

## Ver también

- [Documentación oficial de Gleam](https://gleam.run/documentation/)
- [Repositorio de Gleam en GitHub](https://github.com/gleam-lang/gleam)
- [Canal de Gitter de Gleam](https://gitter.im/gleam-lang/gleam)
- [Servidor de Discord de Gleam](https://discord.gg/KXuKcQD)
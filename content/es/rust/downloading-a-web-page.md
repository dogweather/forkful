---
title:                "Rust: Descargar una página web"
simple_title:         "Descargar una página web"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Por qué descargar una página web en Rust?

Existen muchas razones por las que puede ser útil descargar una página web en Rust. Puede ser para obtener datos de una página de forma automática, para crear una herramienta de raspado web o quizás simplemente por el aprendizaje sobre cómo funciona el proceso de descarga de una página. Sea cual sea la razón, descargar una página web en Rust es una tarea bastante sencilla y en este artículo te mostraré cómo hacerlo.

## Cómo hacerlo en Rust?

Para descargar una página web en Rust, primero debes tener instalado el lenguaje en tu sistema. Puedes seguir las instrucciones de la [página oficial de Rust](https://www.rust-lang.org/es/tools/install) para instalarlo.

Una vez que tengas Rust instalado, debes crear un nuevo proyecto en tu editor de código de preferencia. En este ejemplo, utilizaremos [VS Code](https://code.visualstudio.com/) con la extensión de Rust instalada.

Ahora, en tu proyecto, crea un nuevo archivo llamado "main.rs" y dentro de él, importa las siguientes librerías:
```Rust
use std::io::Read;
use std::fs::File;
use std::io::Write;
```
Estas librerías nos permitirán leer y escribir archivos en Rust.

Luego, dentro de la función principal del archivo, declaramos una variable para almacenar la URL de la página que queremos descargar y una segunda variable para almacenar el nombre del archivo en el que queremos guardar la descarga.
```Rust
let url = "https://www.ejemplo.com/";
let file_name = "index.html";
```

A continuación, creamos una nueva instancia de la estructura URL utilizando la URL que hemos declarado.
```Rust
let parsed_url = Url::parse(url).unwrap();
```

Después, creamos una nueva solicitud HTTP utilizando la librería `reqwest` y la función `get`, pasándole como parámetros la URL que hemos declarado. De esta forma, obtendremos una respuesta del servidor.
```Rust
let mut response = match reqwest::get(parsed_url) {
    Ok(r) => r,
    Err(e) => panic!("Error al conectar a la página: {}", e),
};
```

A continuación, leemos el cuerpo de la respuesta utilizando el método `text` y almacenamos el resultado en una variable nueva.
```Rust
let mut content = String::new();
match response.read_to_string(&mut content) {
    Ok(_) => (),
    Err(e) => panic!("Error al leer el contenido de la página: {}", e),
};
```

Por último, creamos un nuevo archivo utilizando el nombre que hemos declarado y escribimos el contenido descargado utilizando el método `write_all`.
```Rust
let mut file = match File::create(file_name) {
    Ok(f) => f,
    Err(e) => panic!("Error al crear archivo: {}",e),
};

match file.write_all(content.as_bytes()) {
    Ok(_) => println!("Página web descargada con éxito"),
    Err(e) => panic!("Error al escribir en el archivo: {}", e),
};
```

Y eso es todo, ¡ya tienes una página web descargada en Rust! Puedes probarlo cambiando la URL y el nombre del archivo y ejecutando el código.

## Profundizando en la descarga de páginas web

Si quieres profundizar más en el proceso de descarga de páginas web en Rust, te recomiendo investigar sobre las diferentes librerías que existen para manejar solicitudes HTTP y explorar otras formas de guardar el contenido descargado, como por ejemplo utilizando la librería `html5ever` para analizar el contenido HTML y extraer la información deseada.

## Véase también

- [Página oficial de Rust](https://www.rust-lang.org/es/)
- [Documentación de reqwest](https://docs.rs/reqwest/)
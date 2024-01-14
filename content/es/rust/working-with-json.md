---
title:                "Rust: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/working-with-json.md"
---

{{< edit_this_page >}}

# ¿Por qué trabajar con JSON en Rust?

JSON (JavaScript Object Notation) es un formato de intercambio de datos ampliamente utilizado en la actualidad. Es ligero, fácil de leer y escribir, y es compatible con muchos lenguajes de programación, incluido Rust. En este artículo, hablaremos sobre cómo trabajar con JSON en Rust y cómo puede mejorar su experiencia de programación.

## Cómo hacerlo

Para trabajar con JSON en Rust, necesitamos utilizar una biblioteca externa llamada `serde`, que es una poderosa herramienta para serializar y deserializar datos en diferentes formatos, incluido JSON.

Empezaremos creando una estructura en Rust que represente nuestro objeto JSON. Supongamos que tenemos un objeto llamado "persona" con los campos "nombre", "edad" y "país".

```Rust
#[derive(Serialize, Deserialize)]
struct Persona {
    nombre: String,
    edad: u32,
    pais: String,
}
```

Con la ayuda de las anotaciones `Serialize` y `Deserialize`, podemos convertir automáticamente nuestra estructura en un objeto JSON válido.

Ahora, podemos utilizar la biblioteca `serde_json` para codificar y decodificar nuestros datos. Por ejemplo, si queremos codificar nuestro objeto "persona" en JSON:

```Rust
let persona = Persona{
    nombre: String::from("Juan"),
    edad: 35,
    pais: String::from("España"),
};

let json = serde_json::to_string(&persona).unwrap();
```

Y para decodificar un objeto JSON en una estructura de Rust:

```Rust
let json = r#"{"nombre": "Maria", "edad": 28, "pais": "Argentina"}"#;
let persona: Persona = serde_json::from_str(json).unwrap();
```

Como se puede ver, trabajar con JSON en Rust es bastante sencillo gracias a la biblioteca `serde`. Además, podemos manejar errores con el uso de la función `unwrap` o utilizar `Result` en su lugar para manejar adecuadamente los casos de error.

## Profundizando

La biblioteca `serde` también proporciona opciones adicionales para modificar el comportamiento de la serialización y deserialización en caso de que sea necesario. Por ejemplo, podemos utilizar la anotación `#[serde(rename = "new_name")]` para cambiar el nombre de un campo en nuestro objeto JSON.

También es posible trabajar con JSON de forma asincrónica utilizando la biblioteca `serde_json::value`, que nos permite leer y escribir datos JSON utilizando tipos asíncronos.

## Ver también

- [Documentación oficial de `serde`](https://serde.rs/)
- [Documentación oficial de `serde_json`](https://docs.rs/serde_json)

¡Ahora está listo para trabajar con JSON en Rust! Esperamos que este artículo haya sido útil y que puedas aprovechar al máximo la biblioteca `serde`. ¡Hasta la próxima!
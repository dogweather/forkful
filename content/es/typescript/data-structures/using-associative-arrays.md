---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:10.596042-07:00
description: "Los arreglos asociativos, o objetos en TypeScript, te permiten usar\
  \ cadenas (o claves) para acceder a pares de valores. Los programadores los utilizan\u2026"
lastmod: '2024-02-25T18:49:55.292793-07:00'
model: gpt-4-0125-preview
summary: "Los arreglos asociativos, o objetos en TypeScript, te permiten usar cadenas\
  \ (o claves) para acceder a pares de valores. Los programadores los utilizan\u2026"
title: Uso de matrices asociativas
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Los arreglos asociativos, o objetos en TypeScript, te permiten usar cadenas (o claves) para acceder a pares de valores. Los programadores los utilizan para patrones de acceso a datos más dinámicos en comparación con los arreglos tradicionales, proporcionando una manera flexible de estructurar y acceder a datos sin estar atados a índices numéricos.

## Cómo hacerlo:

Crear y usar arreglos asociativos en TypeScript es sencillo. Aquí tienes una guía básica:

```TypeScript
// Declarando un arreglo asociativo
let usuario: { [clave: string]: string } = {};

// Agregando datos
usuario["nombre"] = "Jane Doe";
usuario["correo"] = "jane@example.com";

console.log(usuario);
```

Salida:

```TypeScript
{ nombre: 'Jane Doe', correo: 'jane@example.com' }
```

Iterar sobre pares clave-valor también es fácil:

```TypeScript
for (let clave in usuario) {
    console.log(clave + ": " + usuario[clave]);
}
```

Salida:

```TypeScript
nombre: Jane Doe
correo: jane@example.com
```

Y si estás lidiando con una mezcla de tipos de datos, el sistema de tipos de TypeScript es muy útil:

```TypeScript
let tiposMixtos: { [clave: string]: string | number } = {};
tiposMixtos["nombre"] = "John Doe";
tiposMixtos["edad"] = 30;

console.log(tiposMixtos);
```

Salida:

```TypeScript
{ nombre: 'John Doe', edad: 30 }
```

## Un Vistazo Profundo

En TypeScript, lo que nos referimos como arreglos asociativos son esencialmente objetos. Históricamente, en lenguajes como PHP, los arreglos asociativos son un tipo fundamental, pero JavaScript (y por extensión, TypeScript) usa objetos para este propósito. Este enfoque es tanto una fortaleza como una limitación. Los objetos proporcionan una estructura altamente dinámica para asociar cadenas a valores, pero no están destinados a ser usados como 'arreglos' en el sentido tradicional. Por ejemplo, no puedes usar métodos de arreglo como `push` o `pop` directamente en estos objetos.

Para casos en los que necesitas colecciones ordenadas de pares clave-valor con operaciones estilo arreglo, TypeScript (y JavaScript moderno) ofrece el objeto `Map`:

```TypeScript
let mapaUsuario = new Map<string, string>();
mapaUsuario.set("nombre", "Jane Doe");
mapaUsuario.set("correo", "jane@example.com");

mapaUsuario.forEach((valor, clave) => {
    console.log(clave + ": " + valor);
});
```

Mientras que el sistema de tipos de TypeScript y características de ES6 como `Map` proporcionan alternativas poderosas, entender cómo usar objetos como arreglos asociativos es útil para escenarios donde los literales de objeto son más eficientes o cuando se trabaja con estructuras de datos JSON. Se trata de elegir la herramienta adecuada para el trabajo.

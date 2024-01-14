---
title:                "Arduino: Borrando caracteres que coinciden con un patrón"
simple_title:         "Borrando caracteres que coinciden con un patrón"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

Si estás buscando una forma rápida de eliminar caracteres que coincidan con un patrón en tu código de Arduino, estás en el lugar correcto. En este artículo, te mostraremos cómo hacerlo de manera sencilla y efectiva.

## Cómo hacerlo

Para eliminar caracteres que coincidan con un patrón en Arduino, puedes utilizar la función `removeChars()` y especificar el patrón que deseas eliminar en la entrada. Por ejemplo, si quieres eliminar todos los caracteres que sean vocales, puedes utilizar el siguiente código:

```Arduino
void setup() {
  String texto = "Hola mundo";
  removeChars(texto, "aeiou"); // Elimina las vocales del texto
  Serial.println(texto);
}

void removeChars(String &texto, String pattern) {
  for (int i = 0; i < pattern.length(); i++) {
    texto.replace(pattern[i], "");
  }
}
```

La salida en el monitor serie será "Hl mnd" ya que todas las vocales han sido eliminadas del texto original. Puedes modificar la función `removeChars()` para que se ajuste a tus necesidades, como eliminar números o símbolos específicos.

## Inmersión profunda

Además de la función `removeChars()`, hay otras formas de eliminar caracteres que coincidan con un patrón en Arduino. Una opción es utilizar la librería `StringRemove`, que te permite eliminar caracteres en una cadena de forma similar a la función `removeChars()`. Otra forma es utilizar expresiones regulares con la librería `Regexp`, que te permite buscar y reemplazar patrones en cadenas de texto.

También es importante tener en cuenta que, si tu Arduino cuenta con poca memoria, es posible que tengas problemas al utilizar cadenas de texto largas y funciones de eliminación de caracteres. En este caso, es recomendable encontrar una alternativa utilizando arreglos de caracteres o limitar el uso de cadenas de texto en tu código.

## Ver también

- [Librería StringRemove](https://playground.arduino.cc/Code/StringRemove/)
- [Librería Regexp](https://playground.arduino.cc/Code/Regexp/)
- [Expresiones regulares en Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)

¡Esperamos que este artículo te haya sido útil para aprender a eliminar caracteres que coincidan con un patrón en Arduino! ¡Feliz programación!
---
title:                "Arduino: Buscando y reemplazando texto"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué buscar y reemplazar texto en Arduino

La búsqueda y reemplazo de texto es una técnica útil en la programación de Arduino que te permite realizar cambios rápidos y precisos en grandes cantidades de texto. Ya sea que quieras cambiar un nombre de variable o corregir un error ortográfico, esta herramienta te ahorrará tiempo y esfuerzo en tu proyecto.

## Cómo hacerlo
Para realizar una búsqueda y reemplazo en el código de Arduino, primero debes abrir tu archivo en el IDE de Arduino. Luego, sigue estos pasos:

1. Presiona "Ctrl+F" para abrir la ventana de búsqueda.
2. Escribe la palabra que deseas buscar en el campo "Buscar".
3. En el campo "Reemplazar con", escribe la palabra o frase con la que deseas reemplazar la palabra buscada.
4. Opcionalmente, puedes seleccionar diferentes opciones de búsqueda, como hacerla sensible a mayúsculas o minúsculas.
5. Haz clic en "Reemplazar todo" para realizar los cambios en todo el código.
6. Guarda tu archivo y compila para asegurarte de que los cambios se hayan realizado correctamente.

A continuación, se muestra un ejemplo de código en Arduino con la palabra "hola" que se reemplaza por "mundo" utilizando esta técnica.

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  Serial.println("hola");
  delay(1000);
}
```

Salida:

```
mundo
mundo
mundo
```

## Profundizando en la búsqueda y reemplazo de texto

La búsqueda y reemplazo de texto también admite expresiones regulares, lo que te permite realizar cambios más avanzados en el código. Las expresiones regulares son patrones de búsqueda que pueden incluir caracteres especiales, como comodines o rangos.

Por ejemplo, si deseas reemplazar todas las variables que comienzan con "x" por "y", puedes utilizar la expresión regular "x\w+" en el campo "Buscar" y "y" en el campo "Reemplazar con". Esto cambiará todas las variables "x" seguidas por cualquier combinación de letras por "y".

Ten en cuenta que es importante tener cuidado al utilizar expresiones regulares, ya que pueden causar cambios no deseados si no se utilizan correctamente. Además, es posible que necesites consultar la documentación de Arduino para comprender cómo se manejan las expresiones regulares en su IDE.

## Ver también
- Documentación de búsqueda y reemplazo en Arduino: https://www.arduino.cc/reference/en/language/structure/further-syntax/replace/
- Tutorial de expresiones regulares: https://regexone.com/
---
title:    "Arduino: Borrando caracteres que coincidan con un patrón"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Por qué

Cuando trabajamos en proyectos con Arduino, podemos encontrarnos con la necesidad de eliminar caracteres que coincidan con un patrón específico de una cadena de texto. Esto es especialmente útil cuando queremos obtener datos específicos de una señal o sensor.

## Cómo hacerlo

La función ```.remove(pattern)``` en Arduino nos permite eliminar caracteres que coincidan con un patrón en una cadena. Por ejemplo, si tenemos una señal de temperatura de un sensor que se ve así: "Temp: 25C", y solo queremos obtener el valor numérico, podemos usar esta función para eliminar la palabra "Temp: " y solo obtener "25C". Veamos un ejemplo de código:

```Arduino
String temp = "Temp: 25C";
temp.remove("Temp: ");
Serial.println(temp);  // Output: 25C
```

En este caso, el patrón "Temp: " es eliminado de la cadena original "Temp: 25C" y solo se muestra el valor numérico restante.

## Profundizando

La función ```.remove()``` también acepta expresiones regulares como patrones, lo que nos da más flexibilidad al momento de eliminar caracteres. Por ejemplo, si queremos eliminar todas las letras de una cadena, podemos usar el patrón ```[a-zA-Z]+``` de esta manera:

```Arduino
String str = "Hola123";
str.remove("[a-zA-Z]+");
Serial.println(str);  // Output: 123
```

Además, es importante tener en cuenta que esta función solo elimina los caracteres que coincidan con el patrón, no los reemplaza con nada. Por lo tanto, en el ejemplo anterior, los caracteres eliminados simplemente son eliminados y no se reemplazan con ningún otro carácter.

## Ver también

- [Documentación oficial de la función .remove() en Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/remove/)
- [Tutorial de expresiones regulares en Arduino](https://www.arduino.cc/reference/es/language/structure/further-syntax/regular-expressions/)
- [Video explicativo sobre el uso de expresiones regulares en Arduino](https://www.youtube.com/watch?v=9XaJhiWQmCA)
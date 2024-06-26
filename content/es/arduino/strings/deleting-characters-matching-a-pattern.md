---
date: 2024-01-20 17:41:35.167885-07:00
description: "C\xF3mo: Eliminar caracteres por patr\xF3n no es algo exclusivo de Arduino;\
  \ es un concepto que viene desde los inicios de la programaci\xF3n de computadoras.\u2026"
lastmod: '2024-04-05T21:54:00.664938-06:00'
model: gpt-4-1106-preview
summary: "Eliminar caracteres por patr\xF3n no es algo exclusivo de Arduino; es un\
  \ concepto que viene desde los inicios de la programaci\xF3n de computadoras."
title: "Eliminando caracteres que coinciden con un patr\xF3n"
weight: 5
---

## Cómo:
```Arduino
String eliminarPatron(String str, String patron) {
  String resultado = "";
  for (int i = 0; i < str.length(); i++) {
    if (!patron.indexOf(str[i]) >= 0) {
      resultado += str[i];
    }
  }
  return resultado;
}

void setup() {
  Serial.begin(9600);
  String texto = "Hola, ¿Cómo están? #123";
  String caracteresParaEliminar = " ,?#";
  Serial.println(eliminarPatron(texto, caracteresParaEliminar));
}

void loop() {
  // No es necesario para este ejemplo.
}
```
Salida:
```
HolaCómoestán123
```

## Profundización
Eliminar caracteres por patrón no es algo exclusivo de Arduino; es un concepto que viene desde los inicios de la programación de computadoras. Alternativas incluyen el uso de expresiones regulares en lenguajes como Python o JavaScript, pero en Arduino, por su simplicidad y limitaciones de hardware, se opta por soluciones como la presentada arriba. La implementación puede variar: algunos usan la clase `String`, otros prefieren trabajar con `char` arrays para mejor rendimiento y menor uso de memoria.

## Ver También
- Documentación de Arduino sobre la clase `String`: https://www.arduino.cc/reference/en/language/variables/data-types/string/
- Tutorial de manipulación de `String` en Arduino: https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringAdditionOperator
- Discusión sobre rendimiento de `String` vs `char` arrays en Arduino: https://forum.arduino.cc/t/string-vs-char-array/539038

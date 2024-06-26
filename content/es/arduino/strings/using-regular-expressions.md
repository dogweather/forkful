---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:48.947086-07:00
description: "C\xF3mo: Arduino no tiene soporte incorporado para regex directamente\
  \ en su biblioteca est\xE1ndar. Sin embargo, puedes lograr una funcionalidad similar\
  \ a regex\u2026"
lastmod: '2024-03-13T22:44:59.324476-06:00'
model: gpt-4-0125-preview
summary: "Arduino no tiene soporte incorporado para regex directamente en su biblioteca\
  \ est\xE1ndar."
title: Usando expresiones regulares
weight: 11
---

## Cómo:
Arduino no tiene soporte incorporado para regex directamente en su biblioteca estándar. Sin embargo, puedes lograr una funcionalidad similar a regex para patrones simples utilizando funciones básicas de cadenas de texto, o para necesidades más complejas, integrar una biblioteca de terceros como `regex`.

### Coincidencia de Cadenas Básica sin Regex
Para necesidades básicas, como encontrar una subcadena, puedes usar la función `String.indexOf()`:
```cpp
String data = "Valor del sensor: 12345";
int index = data.indexOf("valor:");
if (index != -1) {
  String value = data.substring(index + 6).trim();
  Serial.println(value); // Imprime: 12345
}
```

### Usar una Biblioteca de Terceros para Regex
Para manejar patrones más complejos, podrías considerar una biblioteca como `regex`. Después de instalar la biblioteca, puedes usarla de la siguiente manera:

1. **Instalación**: La biblioteca `regex` podría no estar disponible directamente en el Administrador de Bibliotecas de Arduino, por lo que podrías necesitar instalarla manualmente descargándola de una fuente de confianza y agregándola a tu carpeta de bibliotecas de Arduino.

2. **Uso de Ejemplo**:
Asumiendo que la biblioteca proporciona funcionalidades similares a las implementaciones estándar de regex, podrías usarla de la siguiente manera:

```cpp
#include <regex.h>

void setup() {
  Serial.begin(9600);
  while (!Serial); // Espera a que Serial esté listo
  
  regex_t reg;
  const char* pattern = "[0-9]+"; // Coincide con una secuencia de dígitos
  regcomp(&reg, pattern, REG_EXTENDED);
  
  const char* test_str = "Valor del sensor: 12345";
  
  regmatch_t matches[1];
  if (regexec(&reg, test_str, 1, matches, 0) == 0) {
    // Extrae e imprime la porción que coincide
    int start = matches[0].rm_so;
    int end = matches[0].rm_eo;
    char match[end-start+1];
    strncpy(match, test_str + start, end-start);
    match[end-start] = '\0';
    
    Serial.print("Coincidencia encontrada: ");
    Serial.println(match); // Imprime: 12345
  } else {
    Serial.println("No se encontró coincidencia");
  }
  
  regfree(&reg); // Libera la memoria asignada para regex
}

void loop() {
  // coloca aquí tu código principal, para ejecutar repetidamente:
}
```

**Nota**: La sintaxis y las funciones específicas utilizadas aquí son con fines ilustrativos y podrían variar según los detalles de implementación reales de la biblioteca `regex` que elijas. Siempre consulta la documentación de la biblioteca para obtener información precisa y actualizada.

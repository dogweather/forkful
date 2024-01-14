---
title:    "C: Escribiendo pruebas"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c/writing-tests.md"
---

{{< edit_this_page >}}

#Por qué escribir pruebas en tus programas C?

Escribir pruebas en tus programas C puede parecer tedioso y una pérdida de tiempo, pero en realidad tiene muchos beneficios. Primero, ayuda a detectar y corregir errores en el código, lo que puede ahorrarte tiempo y frustración a largo plazo. También puede mejorar la calidad y la estabilidad de tu código, lo que a su vez puede aumentar la confianza de los usuarios en tu programa.

## Cómo escribir pruebas en C

Para escribir pruebas en tus programas C, necesitarás un marco de prueba. Un marco de prueba es una herramienta que te ayuda a automatizar el proceso de probar tu código. Hay varios marcos de prueba disponibles para C, pero en este artículo utilizaremos CUnit.

Primero, necesitas descargar e instalar CUnit en tu sistema. Luego, puedes seguir estos pasos para escribir tus pruebas:

1. Incluye la biblioteca CUnit en tu código: 

```C
 #include <CUnit/CUnit.h>
```

2. Declara tus pruebas: 

```C
void test_suma(void);
```

3. Define tus pruebas: 

```C 
void test_suma(void) {
    CU_ASSERT_EQUAL(2+2, 4);
    CU_ASSERT_NOT_EQUAL(3+3, 5);
} 
```

4. Registra tus pruebas: 

```C
CU_pSuite suite = CU_add_suite("Suite de prueba", NULL, NULL);
CU_ADD_TEST(suite, test_suma);
```

5. Ejecuta las pruebas: 

```C
CU_basic_run_tests();
```

6. Verifica los resultados: 

```C
CU_get_failure_list();
```

Con estos pasos, puedes escribir y ejecutar tus pruebas en C y asegurarte de que tu código funciona como se espera.

## Profundizando en las pruebas de C

Además de probar diferentes condiciones y entradas en tu código, también puedes realizar pruebas de regresión para asegurarte de que cualquier cambio que hagas no afectará la funcionalidad del resto de tu programa. También puedes usar diferentes técnicas de prueba, como la prueba basada en propiedad, que te ayudan a identificar áreas de tu código que necesitan más pruebas.

También es importante recordar que las pruebas no garantizan que tu código sea perfecto, pero pueden ayudar a mejorar su calidad y confiabilidad.

#Ver también

- [CUnit - marco de prueba para C](https://sourceforge.net/projects/cunit/)
- [Introducción a pruebas en C](https://youtu.be/oLSd1HSHIrY)
- [Tutorial de CUnit](https://www.cs.colostate.edu/~cs157/CUnit/intro.html)
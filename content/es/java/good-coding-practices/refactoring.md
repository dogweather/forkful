---
date: 2024-01-26 01:38:25.812753-07:00
description: "La refactorizaci\xF3n es el proceso de reestructuraci\xF3n del c\xF3\
  digo inform\xE1tico existente\u2014cambiando la facturaci\xF3n\u2014sin alterar\
  \ su comportamiento externo. Los\u2026"
lastmod: '2024-03-13T22:44:58.945597-06:00'
model: gpt-4-0125-preview
summary: "La refactorizaci\xF3n es el proceso de reestructuraci\xF3n del c\xF3digo\
  \ inform\xE1tico existente\u2014cambiando la facturaci\xF3n\u2014sin alterar su\
  \ comportamiento externo."
title: "Refactorizaci\xF3n"
weight: 19
---

## ¿Qué y por qué?
La refactorización es el proceso de reestructuración del código informático existente—cambiando la facturación—sin alterar su comportamiento externo. Los programadores lo hacen para mejorar los atributos no funcionales del software, mejorando la legibilidad, reduciendo la complejidad y haciendo el código más mantenible para futuros proyectos.

## Cómo hacerlo:
Tomemos una clase Java simple que está gritando por una refactorización debido a su pobre organización y falta de claridad.

```java
public class Calculadora {
    public int calc(int op1, int op2, String operacion) {
        if (operacion.equals("sumar")) {
            return op1 + op2;
        } else if (operacion.equals("restar")) {
            return op1 - op2;
        } // Otras operaciones...
    }
}
```

Después de la refactorización, tenemos:

```java
public class Calculadora {
    public int sumar(int operando1, int operando2) {
        return operando1 + operando2;
    }

    public int restar(int operando1, int operando2) {
        return operando1 - operando2;
    }

    // Otras operaciones...
}
```

Mediante la refactorización, hemos mejorado los nombres de los métodos y parámetros para la legibilidad y eliminado la necesidad de una rama condicional dentro de un método único. Ahora, cada operación declara claramente su propósito.

## Profundización:
La refactorización tiene sus raíces en la comunidad de Smalltalk, con su énfasis en la legibilidad del código y el diseño orientado a objetos, pero realmente despegó en el mundo de Java a finales de los 90 y principios de los 00, particularmente después de la publicación del libro seminal de Martin Fowler, "Refactorización: Mejorando el diseño del código existente."

Existen alternativas a la refactorización, como reescribir el código desde cero. Sin embargo, a menudo se prefiere la refactorización porque implica cambios incrementales que no interrumpen la funcionalidad de la aplicación.

Los detalles de implementación al refactorizar en Java (o cualquier lenguaje de programación) giran en torno a la comprensión de los olores del código—indicadores de problemas más profundos en el código. Algunos olores incluyen métodos largos, clases grandes, código duplicado y uso excesivo de primitivas. Aplicando patrones de refactorización como Extraer Método, Mover Método o Reemplazar Temporal con Consulta, los desarrolladores pueden abordar sistemáticamente estos olores mientras aseguran que el código permanezca funcional en todo momento.

Herramientas automatizadas, como el soporte de refactorización de IntelliJ IDEA, o plugins para Eclipse, pueden ayudar en el proceso automatizando refactorizaciones como renombrar variables, métodos y clases, extrayendo métodos o variables, y moviendo métodos o clases a diferentes paquetes o espacios de nombres.

## Ver también:
- Libro de Martin Fowler "Refactorización: Mejorando el diseño del código existente": https://martinfowler.com/books/refactoring.html
- Técnicas de refactorización en Refactoring.Guru: https://refactoring.guru/es/tecnicas-de-refactorizacion
- Refactorización automatizada en Eclipse: https://www.eclipse.org/eclipse/news/4.18/jdt.php
- Características de refactorización de IntelliJ IDEA: https://www.jetbrains.com/idea/features/refactoring.html

Cada uno de estos recursos proporciona una base para entender los principios de la refactorización o herramientas que pueden ser aprovechadas para poner estos principios en práctica.

---
date: 2024-01-26 04:41:33.442263-07:00
description: "C\xF3mo hacerlo: Java no tiene soporte incorporado para n\xFAmeros complejos,\
  \ pero podemos crear nuestra propia clase o usar una biblioteca. Aqu\xED hay un\
  \ ejemplo\u2026"
lastmod: '2024-03-13T22:44:58.931794-06:00'
model: gpt-4-0125-preview
summary: "Java no tiene soporte incorporado para n\xFAmeros complejos, pero podemos\
  \ crear nuestra propia clase o usar una biblioteca."
title: "Trabajando con n\xFAmeros complejos"
weight: 14
---

## Cómo hacerlo:
Java no tiene soporte incorporado para números complejos, pero podemos crear nuestra propia clase o usar una biblioteca. Aquí hay un ejemplo rápido de cómo crear una sencilla clase `ComplexNumber` y usarla:

```java
public class ComplexNumber {
    private double real;
    private double imaginario;

    public ComplexNumber(double real, double imaginario) {
        this.real = real;
        this.imaginario = imaginario;
    }

    public ComplexNumber add(ComplexNumber other) {
        return new ComplexNumber(this.real + other.real, this.imaginario + other.imaginario);
    }

    // ToString para mostrar números complejos en forma de a + bi
    @Override
    public String toString() {
        return String.format("%.1f + %.1fi", real, imaginario);
    }

    // Prueba rápida
    public static void main(String[] args) {
        ComplexNumber c1 = new ComplexNumber(2, 3);
        ComplexNumber c2 = new ComplexNumber(1, 4);

        System.out.println("Suma: " + c1.add(c2));
    }
}
```

La salida de muestra para el método principal será:

```
Suma: 3.0 + 7.0i
```

## Inmersión profunda
Antes de los lenguajes de alto nivel como Java, los programadores trabajaban directamente con bibliotecas matemáticas en lenguajes como Fortran o C para gestionar operaciones complejas. El concepto se remonta al siglo XVI, acreditado a matemáticos como Gerolamo Cardano y Rafael Bombelli.

En Java, `java.lang.Math` es un lugar predilecto para lo esencial pero omite los números complejos, probablemente porque no todos los programadores los usan. ¿Alternativas? Usar bibliotecas. Apache Commons Math proporciona una clase `Complex` cargada con métodos para su manipulación. Aquí está por qué crear tu propia clase es interesante: Ligera, adaptada a tus necesidades exactas, y sin la sobrecarga de una biblioteca.

Un detalle importante: ¡cuidado con la precisión de punto flotante! Las computadoras no pueden representar algunos números exactamente, lo que lleva a errores de redondeo. ¡Al realizar operaciones complejas repetitivas, estos errores pueden acumularse!

## Ver también
Para inmersiones más profundas y operaciones más complejas, consulta:

- [Apache Commons Math](https://commons.apache.org/proper/commons-math/)
- [La clase Complex de JScience](http://jscience.org/)
- Tutoriales de Oracle sobre [aritmética de punto flotante](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)

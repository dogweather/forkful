---
title:                "Working with complex numbers"
aliases:
- /en/java/working-with-complex-numbers.md
date:                  2024-01-25T03:00:04.580827-07:00
model:                 gpt-4-1106-preview
simple_title:         "Working with complex numbers"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Complex numbers expand the real number line through the addition of an imaginary unit, `i`, where `i^2 = -1`. They're crucial in fields like engineering, physics, and advanced mathematics, where they model phenomena that real numbers can't handle, like electrical currents and signal processing.

## How to:

Java doesn't have built-in support for complex numbers, but we can roll our own class or use a library. Here's a quick example of how to create a simple `ComplexNumber` class and use it:

```java
public class ComplexNumber {
    private double real;
    private double imaginary;

    public ComplexNumber(double real, double imaginary) {
        this.real = real;
        this.imaginary = imaginary;
    }

    public ComplexNumber add(ComplexNumber other) {
        return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
    }

    // ToString to display complex numbers in a + bi form
    @Override
    public String toString() {
        return String.format("%.1f + %.1fi", real, imaginary);
    }

    // Quick test
    public static void main(String[] args) {
        ComplexNumber c1 = new ComplexNumber(2, 3);
        ComplexNumber c2 = new ComplexNumber(1, 4);

        System.out.println("Sum: " + c1.add(c2));
    }
}
```

Sample output for the main method will be:

```
Sum: 3.0 + 7.0i
```

## Deep Dive

Before high-level languages like Java, programmers worked directly with math libraries in languages like Fortran or C to manage complex operations. The concept harks back to the 16th century, credited to mathematicians like Gerolamo Cardano and Rafael Bombelli.

In Java, `java.lang.Math` is a go-to for essentials but skips complex numbers, probably because not every programmer uses them. Alternatives? Use libraries. Apache Commons Math provides a `Complex` class packed with methods for manipulation. Here's why rolling your own is neat though: Lightweight, tailored to your exact needs, and no library overhead.

One important detail: watch out for floating-point precision. Computers can't represent some numbers exactly, leading to rounding errors. When performing repetitive complex operations, these errors can accumulate!

## See Also

For deeper dives and more complex operations, check:

- [Apache Commons Math](https://commons.apache.org/proper/commons-math/)
- [JScience's Complex class](http://jscience.org/)
- Oracle's tutorials on [floating-point arithmetic](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)

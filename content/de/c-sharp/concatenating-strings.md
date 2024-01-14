---
title:    "C#: Verknüpfung von Zeichenketten"
keywords: ["C#"]
---

{{< edit_this_page >}}

# Warum

In der Programmierung werden oft Strings verwendet, um Text darzustellen. Damit die Anwendung dynamischer und flexibler wird, muss es möglich sein, mehrere Strings miteinander zu verbinden oder aneinanderzuhängen. Dieser Prozess wird als String-Konkatenation bezeichnet und ist eine grundlegende Fähigkeit, die jeder Programmierer kennen sollte.

# Wie funktioniert String-Konkatenation in C#

```C#
string firstName = "Max";
string lastName = "Mustermann";

string fullName = firstName + " " + lastName;
Console.WriteLine(fullName);

// Ausgabe: Max Mustermann
```

In C# kann die Konkatenation von Strings auf verschiedene Arten durchgeführt werden. Eine Möglichkeit ist die direkte Verknüpfung der Strings mit dem `+` Operator. Dabei wird ein neuer String erstellt, der aus der Kombination der beiden ursprünglichen Strings besteht.

Es ist auch möglich, die Methode `String.Concat()` zu verwenden, die mehrere Parameter akzeptiert und diese zusammenfügt. Diese Methode kann besonders nützlich sein, wenn mehr als zwei Strings miteinander kombiniert werden sollen.

```C#
string fruit1 = "Apple";
string fruit2 = "Orange";
string fruit3 = "Banana";

string fruits = String.Concat(fruit1, ", ", fruit2, ", ", fruit3);
Console.WriteLine(fruits);

// Ausgabe: Apple, Orange, Banana
```

# Tiefergehender Einblick

In C# gibt es noch eine weitere Möglichkeit, Strings zu konkatenieren: mit dem `StringBuilder`-Objekt. Dies ist besonders effizient, wenn viele Strings kombiniert werden müssen, da dabei nicht jedes Mal ein neuer String erstellt werden muss.

```C#
StringBuilder sb = new StringBuilder();
sb.Append("Hello");
sb.Append(" World!");
Console.WriteLine(sb.ToString());

// Ausgabe: Hello World!
```

Zudem bietet der `StringBuilder` die Möglichkeit, bereits vorhandene Strings zu bearbeiten, anstatt neue zu erstellen. Dies kann Ressourcen sparen und die Geschwindigkeit der Anwendung verbessern.

# Siehe auch

- [Offizielle Microsoft Dokumentation zur String-Konkatenation in C#](https://docs.microsoft.com/en-us/dotnet/csharp/how-to/concatenate-multiple-strings)
- [Erläuterung der Unterschiede zwischen `+` Operator und `String.Concat()` Methode](https://stackoverflow.com/questions/1309981/concatenation-vs-stringbuilder-which-one-is-faster)
- [Weitere nützliche C# String Operationen](https://www.c-sharpcorner.com/article/string-operations-in-c-sharp/)
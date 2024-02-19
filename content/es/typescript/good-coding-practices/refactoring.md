---
aliases:
- /es/typescript/refactoring/
date: 2024-01-26 03:36:43.443808-07:00
description: "Refactorizaci\xF3n es el proceso de reestructurar c\xF3digo inform\xE1\
  tico existente sin cambiar su comportamiento externo. Los programadores lo hacen\
  \ para hacer\u2026"
lastmod: 2024-02-18 23:09:09.711867
model: gpt-4-0125-preview
summary: "Refactorizaci\xF3n es el proceso de reestructurar c\xF3digo inform\xE1tico\
  \ existente sin cambiar su comportamiento externo. Los programadores lo hacen para\
  \ hacer\u2026"
title: "Refactorizaci\xF3n"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Refactorización es el proceso de reestructurar código informático existente sin cambiar su comportamiento externo. Los programadores lo hacen para hacer el código más limpio, mantenible y para reducir la complejidad, lo cual facilita su comprensión para alguien que se sumerge de nuevo.

## Cómo hacerlo:
Considera una función de TypeScript que ha visto días mejores - está algo desordenada y podría usar un poco de amor y cuidado:

```typescript
function userInfo(data: any): string {
    return "User Info: " + data.name + ", " + data.age + ", " + data.email + ";" ;
}
```
Refactorizada, podría lucir así:

```typescript
interface User {
    name: string;
    age: number;
    email: string;
}

function formatUserInfo(user: User): string {
    return `User Info: ${user.name}, ${user.age}, ${user.email};`;
}
```

El segundo ejemplo es más robusto, aprovechando el sistema de tipos de TypeScript con una `interface` para evitar posibles errores en tiempo de ejecución y mejorar la legibilidad.

## Profundización
La refactorización no es un concepto moderno; evolucionó con la programación, volviéndose más formalizada con la publicación del libro de Martin Fowler "Refactoring: Improving the Design of Existing Code" en 1999. Es crucial en un ambiente de desarrollo Ágil, facilitando cambios adaptativos en el código. Algunas alternativas a la refactorización manual incluyen herramientas automatizadas como TSLint o el propio servidor de lenguaje de TypeScript que pueden sugerir o incluso realizar ciertas tareas de refactorización por ti. Los detalles de implementación usualmente implican reconocer "olores de código", como código duplicado, métodos largos o clases grandes, y aplicar patrones para remediar, como extraer métodos, mover a clases más adecuadas o utilizar construcciones más simples. Estos patrones son clave para entender el cómo y por qué de la refactorización.

## Ver también
- [El libro "Refactoring: Improving the Design of Existing Code" de Martin Fowler](https://martinfowler.com/books/refactoring.html)
- [TSLint para análisis estático de código](https://palantir.github.io/tslint/)
- [Entendiendo los Olores de Código](https://refactoring.guru/refactoring/smells)

---
title:    "TypeScript: 连接字符串"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

为什么：当我们需要将多个字符串连接在一起时，使用字符串连接是非常方便的。它可以帮助我们在代码中动态地生成字符串，使得我们的代码更加灵活和可读性更强。

怎么做：首先，我们需要使用加号（+）运算符来连接两个字符串。让我们来看一个简单的例子，在控制台打印出“Hello World”的字符串：

```TypeScript
let hello:string = "Hello";
let world:string = "World";
console.log(hello + " " + world);
```

输出将会是：“Hello World”。我们也可以使用模板字面量（template literals）来连接字符串，使用反引号（`）来包裹字符串，并在其中使用占位符（placeholders）来插入变量。让我们来看一个使用模板字面量的例子：

```TypeScript
let user:string = "John";
let message:string = `Hello ${user}, welcome to our website!`;
console.log(message);
```

输出将会是：“Hello John, welcome to our website！”在这个例子中，我们也可以使用表达式来计算占位符的值。例如，我们可以将一个数值与一个字符串连接起来：

```TypeScript
let num:number = 5;
let message:string = `The result of the calculation is ${num + 5}`;
console.log(message);
```

输出将会是：“The result of the calculation is 10”。

深入了解：在TypeScript中，字符串连接的底层原理是使用字符串对象的concat()方法，它将两个或多个字符串连接成一个新的字符串。这个方法可以接受多个字符串作为参数，并依次将它们连接在一起。让我们来使用concat()方法来实现之前的两个例子：

```TypeScript
let hello:string = "Hello";
let world:string = "World";
console.log(hello.concat(" ", world));

let user:string = "John";
let message:string = "Hello";
message = message.concat(" ", user, ", welcome to our website!");
console.log(message);
```

在这个例子中，我们也可以传递一个数组作为参数，concat()方法将会将数组中的所有元素连接成一个新的字符串。例如：

```TypeScript
let colors:string[] = ["red", "green", "blue"];
let result:string = colors.concat(["yellow", "orange"]);
console.log(result);
```

输出将会是一个包含所有颜色的字符串，类似于：“redgreenblueyelloworange”。

见下文：如果你想了解更多关于字符串处理的知识，请参考以下链接：

- [TypeScript官方文档：字符串连接](https://www.typescriptlang.org/docs/handbook/strings.html#string-concatenation)
- [Codecademy：字符串连接](https://www.codecademy.com/courses/learn-typescript/lessons/using-typescript/exercises/string-concatenation)
- [W3Schools：字符串连接方法](https://www.w3schools.com/jsref/jsref_concat_string.asp)
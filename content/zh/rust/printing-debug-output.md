---
title:                "Rust: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

为什么：打印调试输出是Rust程序员必须掌握的基本技能之一。它可以帮助我们检查程序中的错误，并且在调试过程中提供更多有用的信息。

##为什么

打印调试输出是Rust程序员必须掌握的基本技能之一。它可以帮助我们检查程序中的错误，并且在调试过程中提供更多有用的信息。

##如何做

打印调试输出在Rust中非常简单。我们可以使用`println!`宏来打印任何类型的值。例如：

```Rust
let num = 5;
println!("The value of num is {}", num);
```

这将打印出`The value of num is 5`。我们也可以打印多个变量，并在输出中使用不同的格式控制符。例如：

```Rust
let num1 = 5;
let num2 = 10;
let str = "Hello";
println!("The value of num1 is {} and num2 is {}, and str is {}", num1, num2, str);
```

这将打印出`The value of num1 is 5 and num2 is 10, and str is Hello`。我们还可以使用调试格式控制符`{:?}`来打印复杂的数据类型，如结构体、数组或向量。例如：

```Rust
#[derive(Debug)]
struct Person {
    name: String,
    age: u32,
}

let person = Person {
    name: String::from("John"),
    age: 25,
};
println!("Person details: {:?}", person);
```

这将打印出`Person details: Person { name: "John", age: 25 }`。除了`println!`宏外，我们还可以使用`eprintln!`宏来将输出打印到标准错误输出流，这在处理错误时非常有用。

##深入了解

在Rust中，我们也可以使用`dbg!`宏来打印调试输出。它不仅会打印值，还会将调试信息打印到标准错误输出流中，以便在调试时更轻松地追踪值的来源。例如：

```Rust
let num = 5;
dbg!(num);
```

这将打印出`num = 5`。我们还可以使用`debug_assert!`宏来在调试时检查断言语句，以帮助我们更有效地排除错误。

##另请参阅

- [Rust官方指南-调试输出](https://doc.rust-lang.org/stable/book/ch05-01-defining-structs.html)
- [Rust官方文档-调试宏](https://doc.rust-lang.org/std/macro.dbg.html)
- [Rust官方文档-断言宏](https://doc.rust-lang.org/std/macro.debug_assert.html)
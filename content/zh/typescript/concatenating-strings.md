---
title:                "TypeScript: 连接字符串"
simple_title:         "连接字符串"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

# 为什么要使用TypeScript连接字符串

在编程中，经常会遇到需要将多个字符串拼接在一起的情况。使用TypeScript来连接字符串可以使代码更加清晰和有序。下面我们将介绍如何通过TypeScript来连接字符串，并深入了解这个过程。

## 如何使用TypeScript连接字符串

首先，我们需要使用加号“+”来连接两个字符串，如下所示：

```TypeScript
let greeting = "你好";
let name = "小明";
let message = greeting + name;

console.log(message); //输出：你好小明
```

在上面的代码中，我们首先定义两个字符串，然后使用加号来连接它们并赋值给一个新的变量。最后，通过console.log()方法来输出连接后的字符串。

除了使用加号，我们也可以使用模板字符串来连接字符串。模板字符串使用反引号“`”来表示，变量则使用`${}`来引用，如下所示：

```TypeScript
let num1 = 10;
let num2 = 5;
let result = `数字1为${num1}，数字2为${num2}，它们的和为${num1 + num2}`;

console.log(result); //输出：数字1为10，数字2为5，它们的和为15
```

在模板字符串中，我们可以方便地引用变量，并在其中进行运算。

## 深入了解字符串连接

在TypeScript中，连接字符串时会根据需要自动转换变量的类型。例如，如果一个变量是数字类型，它将会被转换为字符串类型来进行连接。因此，在使用模板字符串时，我们可以方便地在其中进行数值运算。

此外，TypeScript也支持使用“+=”来连接字符串，并将结果赋值给原始变量。例如：

```TypeScript
let hello = "你好";
hello += "，小明";

console.log(hello); //输出：你好，小明
```

值得注意的是，连接一个字符串时，原始变量的值也会改变。

# 参考链接

- [TypeScript教程-字符串](https://www.runoob.com/typescript/typestr-string.html)
- [TypeScript官方文档-字符串](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [TypeScript Handbook-字符串字面量类型](https://www.typescriptlang.org/docs/handbook/literal-types.html#string-literal-types)

# 参见

- [TypeScript教程-变量和数据类型](https://www.runoob.com/typescript/ts-variable-datatypes.html)
- [TypeScript教程-字符串模板](https://www.runoob.com/typescript/ts-template-strings.html)
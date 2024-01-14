---
title:                "Gleam: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## 为什么：打印调试信息的重要性

在编程过程中，经常会遇到一些难以理解的bug。这时，打印调试信息可以帮助我们更快地定位问题所在，从而加快程序的开发和调试过程。

## 如何打印调试信息：
```Gleam
// 假设我们有一个名为add的函数，功能是将两个数相加并返回结果
fn add(a, b) {
  var result = a + b;
  // 打印调试信息，可在终端看到该信息
  dbg("The result of adding", a, "and", b, "is", result);
  return result;
}
```
运行该函数后，我们可以在终端看到输出如下：
```bash
The result of adding 2 and 3 is 5
```
这样我们就能清楚地知道add函数的运行结果，从而帮助我们定位问题。

## 深入了解：
打印调试信息并不仅限于简单地输出变量的值。在Gleam中，我们还可以使用`${}`语法来输出变量的值，并且还可以同时输出多个变量的值。例如：
```Gleam
// 假设我们有两个变量a和b
dbg("The value of a is ${a} and the value of b is ${b}");
```
输出信息将会是：
```bash
The value of a is 你的值 and the value of b is 你的值
```
此外，我们还可以使用`dbg_expr()`函数来打印表达式的值。这在调试复杂的逻辑过程中非常有用。

## 参考链接：
- [Gleam官方文档](https://gleam.run/book/introduction.html)
- [Gleam Github仓库](https://github.com/gleam-lang/gleam)
- [如何为Gleam函数添加调试信息](https://gleam.run/posts/adding-debugging-output-to-gleam-functions.html)

## 参见：更多Gleam相关文章

欢迎访问我们的网站，了解更多Gleam编程相关的知识和技巧。记得每次遇到难以解决的bug时，不妨尝试使用打印调试信息来快速定位问题哦！
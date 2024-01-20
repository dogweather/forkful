---
title:                "将字符串大写化"
html_title:           "Kotlin: 将字符串大写化"
simple_title:         "将字符串大写化"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 什么是字符串首字母大写？为什么程序员要做这件事？
将首字母大写，即使字符串的第一个字母变为大写形式。程序员经常这么做，因为在很多情况下，语法规则要求句子的第一个单词或每个单词的首字母大写。

## 如何操作：
在Kotlin中，`capitalize()`函数可以满足我们的需求。下面的代码示例印证了我的论点：
```Kotlin
fun main() {
    val lowerCase = "hello, world!"
    val result = lowerCase.capitalize()
    println(result)  // 输出: "Hello, world!"
}
```

## 深入了解：
1. 历史背景：首字母大写作为语法规则之一，由古老的西方语言传来。在计算机编程中，这个规则通常应用于标识符（比如变量名、函数名）和用户接口（比如文本显示）的规范和美观。

2. 其他选择： 在某些情况下，`capitalize()`可能无法满足需求。比如，当我们想对字符串中的每个单词首字母大写时，我们可以使用`split(" ").map { it.capitalize() }.joinToString(" ")`来实现。

3. 实现细节： Kotlin中的`capitalize()`函数首先会检查字符串的第一个字符，如果它是小写的，就转换为大写。其余的字符串字符不变。

## 延伸阅读：
* Kotlin官方文档对`capitalize()`的详细解释： [官方文档链接（英文）](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)


* 文字规范和美学在编程中的重要性： [阅读链接（英文）](https://medium.com/@s.a.khan/importance-of-coding-style-and-standards-41941d8225c0)
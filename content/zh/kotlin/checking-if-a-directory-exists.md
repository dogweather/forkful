---
title:                "Kotlin: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 为什么

当我们需要在程序中遍历文件夹或检查特定文件是否存在时，检查目录是否存在就显得非常重要。这可以帮助我们避免不必要的错误，并帮助我们更有效地管理文件和目录。

## 如何操作

首先，在Kotlin中，我们可以使用```File```类来操作文件和目录。要检查目录是否存在，我们可以使用```File.isDirectory()```方法。这将返回一个布尔值（true或false），指示目录是否存在。下面是一个简单的例子：

```Kotlin
fun main() {
    val directory = File("/Users/username/Documents")
    if (directory.isDirectory()) {
        println("目录存在。")
    } else {
        println("目录不存在。")
    }
}
```

运行上述代码后，如果目录存在，则会输出“目录存在。”，否则输出“目录不存在。”

## 深入了解

除了上面的方法外，我们还可以使用```File.exists()```来检查目录或文件是否存在。与```File.isDirectory()```类似，它也返回一个布尔值。但是，区别在于```File.exists()```可以用于检查任何类型的文件，而不仅仅是目录。因此，它更通用，可以用于检查文件或目录的存在。

此外，我们还可以使用```File.list()```来获取目录中的所有文件和子目录的列表并进行遍历。这可以帮助我们更有效地管理文件和目录，并使我们的代码更具可读性。

## 参考资料

- [Kotlin官方文档：操作文件和目录](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)
- [Kotlin语言参考：File操作](https://kotlinlang.org/docs/reference/io-files.html)
- [博客：Kotlin中如何检查文件或目录是否存在](https://blog.kotlin-academy.com/idiomatic-file-system-operations-in-kotlin-9ae81f4aefe6)
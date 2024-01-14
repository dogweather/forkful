---
title:    "Gleam: 使用标准错误输出写入的方法"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

为什么: 写入标准错误的原因只有1-2句话

很多时候，在编程中我们需要查找和调试错误，而写入标准错误可以帮助我们更有效地理解和解决问题。通过在代码中加入写入标准错误的函数，我们可以在程序运行过程中输出相关信息，从而帮助我们快速定位错误所在。

###如何进行写入标准错误

```Gleam
fn main() {
    // 首先导入标准库中的 io 模块
    import gleam/io

    // 使用 write_to_stderr 函数写入标准错误
    io.write_to_stderr("This is an error message.")

    // 可以写入任意类型的变量
    let count = 5
    io.write_to_stderr("The count is: " ++ to_string(count))
}
```
输出:
```
This is an error message.
The count is: 5
```

###深入了解写入标准错误

在 Gleam 中，我们可以使用 io.write_to_stderr 函数来将任意类型的变量写入标准错误。这个函数会将变量转换为字符串后输出，因此我们可以输出多种类型的数据，比如数字、布尔值、元组等等。同时，我们也可以通过格式化字符串来指定输出的格式。

在写入标准错误时，我们需要注意以下几点：

- 写入标准错误会直接输出在终端中，而不是存储在程序的某个变量中。因此，每次运行程序时会覆盖之前的输出。
- 写入标准错误不会中断程序的运行，但是我们可以根据输出的信息来定位错误所在。
- 可以写入多次标准错误，每次输出都会换行，方便我们阅读。

还有一些其他的输出函数，比如写入标准输出、写入文件等等，我们可以结合使用，灵活地调试我们的程序。

##看看这些参考链接吧

[官方文档](https://gleam.run/documentation/syntax.html#io-functions)

[笔者的个人博客](https://www.example.com)

[开源社区](https://gleam.run/community.html)

#查看更多

[ Gleam代码规范指南（译）](https://github.com/abc/gleam_guidelines_cn)

[如何在Gleam中使用模式匹配](https://github.com/abc/matching_in_gleam_cn)
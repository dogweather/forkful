---
title:                "Gleam: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

为什么：提取子字符串可以帮助简化字符串操作，提高代码的可读性和可维护性。

如何：
提取子字符串是通过使用字符串的索引来截取需要的部分，在Gleam中可以通过使用`String.slice`函数来实现。例如，如果我们有一个字符串`"Hello World!"`，想要提取前五个字符，即`"Hello"`，可以通过以下代码实现：

```Gleam
let greeting = "Hello World!"
let first_five = String.slice(greeting, 0, 5)
```

输出结果为`"Hello"`。我们也可以通过指定负数索引来从后往前截取，例如`String.slice(greeting, -6, -1)`将会得到`"World"`。

深入了解：
提取子字符串并不仅限于固定的索引值，我们可以通过使用变量来动态地提取。例如，如果我们有一个需要处理的字符串列表`["user1", "user2", "user3"]`，我们可以使用循环来一次提取每个字符串中的后缀数字，从而得到一个新的列表`[1, 2, 3]`。代码示例如下：

```Gleam
let users = ["user1", "user2", "user3"]
let suffix = "user"
let ids = for user in users {
    let id_string = String.slice(user, String.length(suffix), String.length(user))
    String.to_int(id_string)
}
```

输出结果为`[1, 2, 3]`。通过这种方法，我们可以更灵活地提取子字符串，处理复杂的字符串操作。

另外，Gleam还提供了更多有用的函数，如`String.trim`来去除字符串两侧的空格，`String.replace`来替换字符串中的特定部分等等。

查看也可以参考的链接：

- [Gleam官方文档](https://gleam.run/documentation/)
- [Gleam字符串操作函数](https://gleam.run/documentation/stdlib/string.html)
- [Gleam列表操作](https://gleam.run/documentation/stdlib/list.html)
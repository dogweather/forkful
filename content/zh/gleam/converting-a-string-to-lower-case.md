---
title:                "将字符串转换为小写"
html_title:           "Arduino: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
将字符串转换为小写是通过编程来将所有大写字符修改为小写字符的过程。程序员这样做主要是为了实现数据的规范化和一致性，避免因大小写差异引起的数据混乱。

## 如何执行：
这是Gleam语言转换字符串为小写的一种方式：
```Gleam
import gleam/string

fn to_lower() {
  let my_string = "HELLO, WORLD!"
  string.to_lower(my_string)
}

// 输出： "hello, world!"
```
上述代码中，我们首先导入了gleam/string模块，然后定义了一个to_lower函数，用于将my_string这个变量的值转换为小写。最后，通过调用string.to_lower(my_string)，将"HELLO, WORLD!"转换为"hello, world!"。

## 深入研究
历史背景：在计算的早期，处理大小写字母通常涉及单独处理每个字符，这是一种非常繁琐的过程。随着编程语言的发展，现在已经有了内置函数可以直接实现这个功能。

替代方案：在Gleam中，除了string.to_lower方法之外，我们还可以使用pattern matching和条件函数来实现字符串的小写转换。

实施细节：Gleam的string.to_lower函数是通过与每个字母的ASCII值进行比较，然后将每个大写字母的ASCII值增加32（因为在ASCII表中，每个小写字母比其相应的大写字母的ASCII值大32）来实现的。

## 参考文献
1. Gleam的string模块官方文档：https://hexdocs.pm/gleam_stdlib/gleam/string.html 
2. ASCII表：https://www.asciitable.com/  
3. 关于字符串大小写转换的详细教程：https://www.programming-idioms.org/idiom/12/convert-a-string-to-lower-case/605/gleam
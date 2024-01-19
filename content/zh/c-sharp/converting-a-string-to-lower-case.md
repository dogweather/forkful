---
title:                "将字符串转换为小写"
html_title:           "Arduino: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

---

## 什么和为什么?

将字符串转换为小写是一个常用的编程任务，它将字符串中的所有大写字母转换为小写字母。程序员这样做主要是为了使数据输入无效，避免大小写问题导致的错误。

## 如何操作:

以下是一个将字符串转换为小写的基本C#代码示例。

```C#
string str = "Hello World!!";
string lowerStr = str.ToLower();
Console.WriteLine(lowerStr);
```

运行以上代码，将会打印出“hello world!!”。

## 深度挖掘:

1. 历史背景: 在早期的编程语言中，并没有内置的方法用于把字符串转换为小写，程序员需要手动来进行这一操作。但随着语言的发展，像C#这样的现代语言已经为此提供了内置功能。

2. 替代方案: 除了ToLowerCase()方法，我们还可以使用LINQ轻松地将字符串转换为小写，如下面的示例所示:

    ```C#
    string str = "Hello World!!";
    string lowerStr = new string(str.Select(c => Char.ToLower(c)).ToArray());
    Console.WriteLine(lowerStr);
    ```

3. 实现细节：`ToLower()`方法的工作方式是通过查找字符的Unicode值并找到相应的小写字母。

## 参考资料:

1. Microsoft Official Documentation on ToLower method: [https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)

2. StackOverflow discussion on converting strings to lowercase: [https://stackoverflow.com/questions/1805796/how-do-i-convert-a-string-to-lower-case](https://stackoverflow.com/questions/1805796/how-do-i-convert-a-string-to-lower-case).

---
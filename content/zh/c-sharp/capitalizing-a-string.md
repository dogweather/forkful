---
title:                "字符串大写化"
html_title:           "C#: 字符串大写化"
simple_title:         "字符串大写化"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##什么和为什么？

字符串大写是指将字符串中的所有字母从小写转换为大写。程序员这样做的原因通常是为了统一字符串的格式，从而方便比较、排序和其他操作。

##怎么做？

###在C#中使用.ToUpper()方法

```
string str = "hello world";
string uppercaseStr = str.ToUpper();
Console.WriteLine(uppercaseStr);
```

输出：
```
HELLO WORLD
```

###使用循环和ASCII码来大写字符串

```
string str = "hello world";
string uppercaseStr = "";

for (int i = 0; i < str.Length; i++)
{
    // 使用ASCII码将小写字母转换为大写字母
    if ((int)str[i] >= 97 && (int)str[i] <= 122)
    {
        uppercaseStr += (char)((int)str[i] - 32);
    }
    else // 非字母字符保持不变
    {
        uppercaseStr += str[i];
    }
}

Console.WriteLine(uppercaseStr);
```

输出：
```
HELLO WORLD
```

##深入研究

历史背景：在早期的计算机系统中，字符串是以ASCII码来表示的，其中小写字母和大写字母之间的差距为32。在那个时候，程序员使用ASCII码来转换字符串大小写。

替代方法：除了使用循环和ASCII码来大写字符串外，我们也可以使用正则表达式来实现。不过，正则表达式的性能可能会受到影响，因此在处理大量数据时，最好使用循环和ASCII码方法。

实现细节：在C#中，字符串是不可变的，所以每当我们对字符串进行任何操作时，实际上是创建了一个新的字符串。因此，使用循环和ASCII码的方法相比正则表达式可能会更有效率。

##参考资料

- [C# 文字列のクリーンな大文字変換](https://kuroeveryday.blogspot.com/2011/10/cc.html)
- [C# - 小文字を大文字に変換する](https://openbook4.me/blog/csharp/to-upper/)
- [C# 字符串大写的不同方法](https://blog.csdn.net/weixin_42221738/article/details/89433544)
---
title:    "Python: 将字符串转换为小写"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么要将字符串转换为小写

字符串是计算机编程中的重要概念，它是由一系列字符组成的。有时，我们需要统一字符串的格式，例如将所有的字母转换为小写。这样做可以使得我们的程序更加健壮和高效。

## 如何将字符串转换为小写

```Python
# 使用lower()方法
str = "Hello World"
new_str = str.lower()
print(new_str)

# 输出：hello world
```
使用lower()方法可以将字符串转换为小写形式，它会返回一个新的字符串，因此在最后一行我们需要使用print()函数来打印出转换后的结果。

## 深入了解字符串转换为小写的过程

在计算机中，每一个字符都会有一个对应的ASCII码。小写字母和大写字母的ASCII码相差32，因此将大写字母加上32就可以得到对应的小写字母。

在Python中，字符串是不可变的数据类型，因此在使用lower()方法转换字符串时，实际上是创建了一个新的字符串，而原来的字符串并没有改变。

## 参考资料

[Python字符串转换为小写](https://www.runoob.com/python3/python3-string-lower.html)

[ASCII码表](https://baike.baidu.com/item/ASCII/309296?fr=aladdin)

## 参见

[Python字符串教程](https://www.runoob.com/python3/python3-string.html)

[Python字符串格式化](https://www.runoob.com/python3/python3-string-format.html)
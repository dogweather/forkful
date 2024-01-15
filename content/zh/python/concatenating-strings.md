---
title:                "串联字符串"
html_title:           "Python: 串联字符串"
simple_title:         "串联字符串"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么

Python中提供了一个方法来连接字符串，即将多个字符串合并为一个字符串。这在日常编程中非常有用，可以方便地构建动态的文本信息，如打印输出、提示信息等。

## 怎么做

```Python
# 使用加号（+）来连接字符串
str1 = "Hello"
str2 = "World"
result = str1 + str2
print(result)
# Output: HelloWorld

# 使用" ".join()方法来连接字符串
str_list = ["Hello", "World"]
result = " ".join(str_list)
print(result)
# Output: Hello World

# 使用格式化字符串来连接字符串
name = "John"
age = 22
result = f"My name is {name} and I am {age} years old."
print(result)
# Output: My name is John and I am 22 years old.
```

## 深入了解

当我们使用加号（+）来连接字符串时，Python会自动在内存中创建一个新的字符串对象，将两个字符串的值拼接起来，然后将新的字符串对象赋值给变量。因此，使用加号来连接大量的字符串可能会导致性能下降。

另一种方法是使用" ".join()来连接字符串，在这种情况下，Python会创建一个字符串列表，然后合并列表中的所有字符串，并在每个字符串之间插入指定的分隔符。

最后，我们还可以使用格式化字符串来连接字符串，它允许我们在字符串中插入变量，从而实现更灵活的文本输出。

## 参考资料

- [Python字符串操作的官方文档](https://docs.python.org/3/library/string.html)
- [关于字符串连接的更多例子和解释](https://realpython.com/python-strings/)

## 参见

- [Python文档](https://www.python.org/)
- [如何学习Python](https://www.geeksforgeeks.org/how-to-learn-python-3/)
- [使用Python构建动态网站](https://realpython.com/python-web-development-intro/)
---
title:                "查找字符串的长度"
html_title:           "Javascript: 查找字符串的长度"
simple_title:         "查找字符串的长度"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何何与为什么?
在编程中，计算字符串的长度就是要找出字符串由多少字符组成。这很有用，当我们想要检查用户输入符合特定要求时 --- 例如，检查密码长度是否满足安全要求。

## 如何操作:
在Java中，我们有一个内置方法`length()`，可以用来找出字符串的长度。让我们看几个示例:
```Java
public class Main {
    public static void main(String[] args) {
        String myString = "你好，世界!";
        int length = myString.length();

        System.out.println("字符串长度为 : " + length);
    }
}
```
输出结果是:
```Java
字符串长度为 : 6
```
在这个例子中，"你好，世界!"包含6个字符，包括标点和空格。

## 深入探讨:
早在Java出现之前，程序员都需要手动计算字符串的长度。随着Java的出现以及`length()`方法的引入，这个问题得到了很好的解决。加上Java为Unicode字符提供了内置支持，`length()`方法能提供准确的字符数量，无论字符是英文单字母还是其它语言的复杂字母。然而，对于特定的使用场景如计算图形字符串或排版文本，可能会需要使用到替代方案，例如 Apache Commons Lang的`StringUtils`库。

## 另见:
- [Java String length()方法](https://www.w3schools.com/java/ref_string_length.asp)
- [Apache Commons Lang](https://commons.apache.org/proper/commons-lang/)
- [Unicode和Java](https://docs.oracle.com/javase/tutorial/i18n/text/unicode.html)
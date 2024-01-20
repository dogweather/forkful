---
title:                "提取子字符串"
html_title:           "Arduino: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
子字符串提取是从现有字符串中抽出一部分的过程。程序员经常这么做，因为它可以截取重要信息，而且在处理文本时非常有用。

## 如何：
### 提取子字符串：
在Java中，你可以使用字符串对象的`.substring()`方法来提取子字符串。在代码段`someString.substring(int beginIndex, int endIndex)`中，`beginIndex`是子字符串的开始位置，`endIndex`是结束位置。

```Java
public class Main {
    public static void main(String[] args) {
        String str = "Hello, World!";
        String substr = str.substring(7, 12);
        System.out.println(substr); // 输出 "World"
    }
}
```

### 提取子字符串（不带结束索引）：
如果想从某个位置开始，直到字符串末尾进行提取，只需要一个参数。

```Java
public class Main {
    public static void main(String[] args) {
        String str = "Hello, World!";
        String substr = str.substring(7);
        System.out.println(substr); // 输出 "World!"
    }
}
```

## 深探
### 历史背景:
`.substring()`方法是Java 1.0版本的一部分，被广泛接受和使用。这是因为字符串处理对于编程来说是核心任务之一，而提取子字符串是处理大量文本的常见需求。

### 可选方法：
除了`.substring()`，还有其他方法可以完成相似的任务，如使用`StringBuilder`或`StringBuffer`类的`.delete()`函数。

```Java
public class Main {
    public static void main(String[] args) {
        StringBuffer str = new StringBuffer("Hello, World!");
        str.delete(0, 7);
        System.out.println(str); // 输出 "World!"
    }
}
```

注意，与`.substring()`方法不同，此操作会直接修改原始字符串缓冲区。

### 实现细节：
`substring()`函数的运行时间复杂度是O(1)，这是因为它在内部只返回字符串的一个引用，而不复制所有字符。这使得它比修改原始字符串或生成新的字符串更有效。

## 另请参阅
1. Oracle官方文档中的[String API](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)提供了清晰的`.substring()`方法说明。
2. StackOverflow上的讨论，解释了[Java中.substring()的工作方式](https://stackoverflow.com/questions/4679746/how-does-string-substring-work-in-java)。
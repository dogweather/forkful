---
title:                "将字符串转化为大写"
html_title:           "Java: 将字符串转化为大写"
simple_title:         "将字符串转化为大写"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 什么以及为什么？
字符串大写是指把字符串的每一个单词的首字母转换成大写。程序员之所以这样做是为了更好地格式化和展示数据。

## 如何做：
```Java
public class Main {
    public static void main(String[] args) {
        String myStr = "hello, world!";
        String capitalizedStr = myStr.substring(0, 1).toUpperCase() + myStr.substring(1);
        System.out.println(capitalizedStr);
    }
}
```
输出结果：
```Java
Hello, world!
```

## 深入探讨
在Java历史的发展中，处理字符串大小写的需求一直存在。对于大写化字符串，可以有多种替代方案。 

1. 使用StringBuilder/StringBuffer的append方法将每个单词的首字母大写。 
2. 使用Apache Commons库的WordUtils.capitalize方法。 

在Java中，这些都可以实现相同目标。但是，内部实现会因为采用的数据结构和期望的决策性能而有所不同。 

## 可参考资料
* [Oracle官方文档：toUpperCase()](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html#toUpperCase())
* [Apache Commons工具：StringUtils](https://commons.apache.org/proper/commons-lang/javadocs/api-release/org/apache/commons/lang3/StringUtils.html)
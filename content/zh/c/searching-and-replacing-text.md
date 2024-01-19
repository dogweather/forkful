---
title:                "搜索和替换文本"
html_title:           "Kotlin: 搜索和替换文本"
simple_title:         "搜索和替换文本"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 什么和为什么?
搜索和替换文本是计算机程序中常见的任务，可以在字符串中寻找特定模式或字词，并将其替换为新的内容。程序员这样做是为了实现数据的更新、清理和重新格式化。

## 如何做:
在C语言中，我们可以使用`strtok`, `strstr`和`sprintf`函数来搜索和替换文本。这是一段示例代码，其中搜索和替换 "dogs" 为 "cats"。

```C
#include <string.h>
#include <stdio.h>

void replace_char(char *str, char *orig, char *rep) {
    static char buffer[4096];
    char *p;

    p = strstr(str, orig);
    
    if (p == NULL) {
        printf("%s", str);
    } else {
        strncpy(buffer, str, p-str);
        buffer[p-str] = '\0';

        sprintf(buffer+(p-str), "%s%s", rep, p+strlen(orig));
        printf("%s", buffer);
    }
}

int main () {
    char string[] = "I love dogs.";
    replace_char(string, "dogs", "cats");

    return(0);
}
```

这个程序的输出将会是:

```C
I love cats.
```

## 深入研究
搜索和替换文本的历史可以追溯到早期的文本处理系统，如UNIX的编辑器`sed`。它允许用户在整个文本文件中搜索并替换任何模式。

在C语言中，我们使用标准库函数来实现搜索和替换。但是，也有更复杂的方法，如使用正则表达式。

另一个可选择的C语言库是`PCRE(Perl Compatible Regular Expressions)`，它提供了对正则表达式的强大支持。但需要注意的是，使用PCRE可能会增加您的代码的复杂性和使用的空间。

## 参考资料
1. [`strtok` - C++ Reference](http://www.cplusplus.com/reference/cstring/strtok/)
2. [`strstr` - C++ Reference](http://www.cplusplus.com/reference/cstring/strstr/)
3. [`sprintf` - C++ Reference](http://www.cplusplus.com/reference/cstdio/sprintf/)
4. [Regular Expressions in C: PCRE](https://www.pcre.org/)
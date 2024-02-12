---
title:                "字符串大写化"
aliases:
- /zh/java/capitalizing-a-string.md
date:                  2024-02-03T19:05:43.989328-07:00
model:                 gpt-4-0125-preview
simple_title:         "字符串大写化"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？
字符串的首字母大写涉及修改字符串中每个单词的首字母为大写，同时确保其余的字母保持小写。这种常见的字符串操作任务对于格式化应用程序中的文本非常有用，比如按照约定或语法正确性准备用户名称或标题。

## 如何操作：
Java的标准库没有提供直接一次性将整个字符串大写的方法，但你可以结合内置方法来实现这一目标。对于更复杂的需求，第三方库像Apache Commons Lang提供了直接的解决方案。

### 使用Java的内置方法
不使用外部库来大写字符串，你可以将字符串分割成单词，将每个单词的首字母大写，然后再将它们重新连接。这里有一个简单的方法：

```java
public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = capitalizeWords(text);
        System.out.println(capitalizedText); // 输出: "Hello, World!"
    }

    public static String capitalizeWords(String str) {
        char[] chars = str.toLowerCase().toCharArray();
        boolean found = false;
        for (int i = 0; i < chars.length; i++) {
            if (!found && Character.isLetter(chars[i])) {
                chars[i] = Character.toUpperCase(chars[i]);
                found = true;
            } else if (Character.isWhitespace(chars[i]) || chars[i]=='.' || chars[i]=='\'') { 
                found = false;
            }
        }
        return String.valueOf(chars);
    }
}
```

这段代码将整个字符串转换为小写，然后遍历每个字符，将每个单词的首字母大写。它将空格、句号和撇号视为单词分隔符。

### 使用Apache Commons Lang

Apache Commons Lang库使用`WordUtils.capitalizeFully()`方法提供了更优雅的解决方案，这个方法为你处理了各种边缘情况和分隔符：

```java
// 添加依赖: org.apache.commons:commons-lang3:3.12.0

import org.apache.commons.text.WordUtils;

public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = WordUtils.capitalizeFully(text);
        System.out.println(capitalizedText); // 输出: "Hello, World!"
    }
}
```

要使用这个方法，你需要将Apache Commons Lang库添加到你的项目中。这个库方法不仅将每个单词的首字母大写，还将每个单词中其余的字母转换为小写，确保整个字符串中的大小写模式一致。

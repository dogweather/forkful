---
title:                "从字符串中移除引号"
date:                  2024-01-26T03:37:37.277994-07:00
model:                 gpt-4-0125-preview
simple_title:         "从字符串中移除引号"

category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么？
从字符串中删除引号意味着剥离文本中包含的任何单引号（`'`）或双引号（`"`）字符。程序员常常这样做是为了清理输入，为比较字符串做准备，或处理可能意外包含引号作为字符串内容部分的文本数据。

## 如何操作：
要在Arduino中删除字符串的引号，您可以遍历字符并重建没有引号字符的字符串。例如：

```arduino
String removeQuotes(String str) {
  String result = ""; // 创建一个空字符串来保存结果
  for (int i = 0; i < str.length(); i++) {
    if (str[i] != '"' && str[i] != '\'') { // 检查每个字符
      result += str[i]; // 如果不是引号，则追加到结果中
    }
  }
  return result;
}

void setup() {
  Serial.begin(9600);
  String testStr = "'Hello, World!'";
  Serial.println(removeQuotes(testStr)); // 应该打印：Hello, World!
}

void loop() {
  // 这里无需做任何事
}
```

串行监视器上的示例输出将是：
```
Hello, World!
```

## 深入探讨
从字符串中移除字符的概念并不独特于Arduino；它在许多编程环境中很常见。从历史上来看，字符串操作函数一直是编程语言的核心部分，以便开发人员有效地清理和解析数据。

除了如上所示地手动遍历和构建新字符串外，还有其他方法。例如，可以使用`replace()`方法用空字符串替换引号，尽管在可读性和管理转义字符方面存在权衡。

```arduino
String removeQuotes(String str) {
  str.replace("\"", ""); // 替换所有双引号
  str.replace("\'", ""); // 替换所有单引号
  return str;
}
```

理解这些权衡至关重要。循环方法对于长字符串来说可能较慢，但它是显式的且易于自定义（如若您需要只移除首尾的引号）。`replace()`方法更简洁，通常也更快，但如果需要处理字符串内部的转义引号字符，则会变得更加复杂。

## 另请参阅
- Arduino字符串参考：https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- W3Schools 关于C++字符串操作的指南（与Arduino的语言相关）：https://www.w3schools.com/cpp/cpp_strings.asp
- Stack Overflow上关于C++（Arduino的基础语言）字符串操作的讨论：https://stackoverflow.com/questions/tagged/string+cpp

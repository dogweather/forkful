---
date: 2024-01-26 03:37:37.277994-07:00
description: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u5220\u9664\u5F15\u53F7\u610F\u5473\u7740\
  \u5265\u79BB\u6587\u672C\u4E2D\u5305\u542B\u7684\u4EFB\u4F55\u5355\u5F15\u53F7\uFF08\
  `'`\uFF09\u6216\u53CC\u5F15\u53F7\uFF08`\"`\uFF09\u5B57\u7B26\u3002\u7A0B\u5E8F\u5458\
  \u5E38\u5E38\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u6E05\u7406\u8F93\u5165\uFF0C\u4E3A\
  \u6BD4\u8F83\u5B57\u7B26\u4E32\u505A\u51C6\u5907\uFF0C\u6216\u5904\u7406\u53EF\u80FD\
  \u610F\u5916\u5305\u542B\u5F15\u53F7\u4F5C\u4E3A\u5B57\u7B26\u4E32\u5185\u5BB9\u90E8\
  \u5206\u7684\u6587\u672C\u6570\u636E\u3002"
lastmod: '2024-03-13T22:44:48.049309-06:00'
model: gpt-4-0125-preview
summary: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u5220\u9664\u5F15\u53F7\u610F\u5473\u7740\
  \u5265\u79BB\u6587\u672C\u4E2D\u5305\u542B\u7684\u4EFB\u4F55\u5355\u5F15\u53F7\uFF08\
  `'`\uFF09\u6216\u53CC\u5F15\u53F7\uFF08`\"`\uFF09\u5B57\u7B26\u3002\u7A0B\u5E8F\u5458\
  \u5E38\u5E38\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u6E05\u7406\u8F93\u5165\uFF0C\u4E3A\
  \u6BD4\u8F83\u5B57\u7B26\u4E32\u505A\u51C6\u5907\uFF0C\u6216\u5904\u7406\u53EF\u80FD\
  \u610F\u5916\u5305\u542B\u5F15\u53F7\u4F5C\u4E3A\u5B57\u7B26\u4E32\u5185\u5BB9\u90E8\
  \u5206\u7684\u6587\u672C\u6570\u636E\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7"
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

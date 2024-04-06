---
date: 2024-01-20 17:41:38.436762-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u4ECE\u65E9\u671F\u7F16\u7A0B\
  \u5F00\u59CB\uFF0C\u5904\u7406\u6587\u672C\u5C31\u662F\u5E38\u89C1\u4EFB\u52A1\u3002\
  \u5728 Arduino \u5E73\u53F0\u4E0A\uFF0C\u8D44\u6E90\u53D7\u9650\uFF0C\u6CA1\u6709\
  \u6B63\u5219\u8868\u8FBE\u5F0F\u5E93\u6765\u7B80\u5316\u6A21\u5F0F\u5339\u914D\uFF0C\
  \u6240\u4EE5\u8981\u624B\u52A8\u68C0\u67E5\u6BCF\u4E2A\u5B57\u7B26\u3002 \u66FF\u4EE3\
  \u65B9\u6848\u5305\u62EC\u4F7F\u7528\u73B0\u6210\u7684\u51FD\u6570\u5E93\uFF08\u5982\
  \u679C\u6709\u8DB3\u591F\u7684\u7A7A\u95F4\uFF09\uFF0C\u4F8B\u5982 `String` \u7C7B\
  \u81EA\u5E26\u7684 `replace()`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.253994-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u4ECE\u65E9\u671F\u7F16\u7A0B\u5F00\u59CB\
  \uFF0C\u5904\u7406\u6587\u672C\u5C31\u662F\u5E38\u89C1\u4EFB\u52A1\u3002\u5728 Arduino\
  \ \u5E73\u53F0\u4E0A\uFF0C\u8D44\u6E90\u53D7\u9650\uFF0C\u6CA1\u6709\u6B63\u5219\
  \u8868\u8FBE\u5F0F\u5E93\u6765\u7B80\u5316\u6A21\u5F0F\u5339\u914D\uFF0C\u6240\u4EE5\
  \u8981\u624B\u52A8\u68C0\u67E5\u6BCF\u4E2A\u5B57\u7B26\u3002"
title: "\u5339\u914D\u6A21\u5F0F\u5220\u9664\u5B57\u7B26"
weight: 5
---

## How to: (如何操作：)
```Arduino
// 示例：删除字符串中的数字
String removeDigits(String str) {
  String result = ""; // 创建新字符串存放结果
  for (int i = 0; i < str.length(); i++) {
    if (!isDigit(str.charAt(i))) { // 检查字符是否不是数字
      result += str.charAt(i); // 如果不是数字，就加到结果字符串
    }
  }
  return result; // 返回结果
}

void setup() {
  Serial.begin(9600); // 开始串口通信
  String original = "A1B2C3D4"; // 原始字符串
  Serial.println("Original: " + original); // 打印原始字符串
  String cleaned = removeDigits(original); // 删除数字
  Serial.println("Without digits: " + cleaned); // 打印结果
}

void loop() {
  // 这里不需要代码
}
```

输出结果将是：

```
Original: A1B2C3D4
Without digits: ABCD
```

## Deep Dive (深入探究：)
从早期编程开始，处理文本就是常见任务。在 Arduino 平台上，资源受限，没有正则表达式库来简化模式匹配，所以要手动检查每个字符。

替代方案包括使用现成的函数库（如果有足够的空间），例如 `String` 类自带的 `replace()` 函数，但这不太适用于模式匹配，只能替换特定字符串。

实现细节里，上述代码示例采用最基础的方法，也是最稳定的方法，在没有复杂库支持时仍然适用。

## See Also (另见：)
- Arduino String 类参考: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- C++ 标准模板库（STL） string 类参考, 适用于较复杂应用: http://www.cplusplus.com/reference/string/string/
- 关于 Arduino 文本处理的社区讨论: https://forum.arduino.cc/index.php?board=7.0

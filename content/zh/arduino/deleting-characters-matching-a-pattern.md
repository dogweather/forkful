---
title:                "匹配模式删除字符"
date:                  2024-01-20T17:41:38.436762-07:00
model:                 gpt-4-1106-preview
simple_title:         "匹配模式删除字符"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (什么和为什么？)

删除匹配模式的字符，就是去掉字符串中符合特定规则的字符。程序员这么做为了数据清洗、格式统一，或减少不必要的信息。

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

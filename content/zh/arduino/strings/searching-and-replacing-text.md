---
aliases:
- /zh/arduino/searching-and-replacing-text/
date: 2024-01-20 17:58:03.755266-07:00
description: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C\u5C31\u662F\u5728\u5B57\u7B26\
  \u4E32\u4E2D\u627E\u7279\u5B9A\u7684\u5B57\u7B26\u6216\u8BCD\u6C47\uFF0C\u5E76\u628A\
  \u5B83\u4EEC\u6362\u6210\u522B\u7684\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\
  \u4E3A\u4E86\u5FEB\u901F\u66F4\u65B0\u4EE3\u7801\u3001\u4FEE\u6539\u914D\u7F6E\u6587\
  \u4EF6\u6216\u8005\u6539\u8FDB\u6570\u636E\u683C\u5F0F\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.356640
model: gpt-4-1106-preview
summary: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C\u5C31\u662F\u5728\u5B57\u7B26\
  \u4E32\u4E2D\u627E\u7279\u5B9A\u7684\u5B57\u7B26\u6216\u8BCD\u6C47\uFF0C\u5E76\u628A\
  \u5B83\u4EEC\u6362\u6210\u522B\u7684\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\
  \u4E3A\u4E86\u5FEB\u901F\u66F4\u65B0\u4EE3\u7801\u3001\u4FEE\u6539\u914D\u7F6E\u6587\
  \u4EF6\u6216\u8005\u6539\u8FDB\u6570\u636E\u683C\u5F0F\u3002"
title: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C"
---

{{< edit_this_page >}}

## What & Why? (是什么与为什么?)
搜索和替换文本就是在字符串中找特定的字符或词汇，并把它们换成别的。程序员这样做是为了快速更新代码、修改配置文件或者改进数据格式。

## How to: (如何操作：)
```Arduino
String originalText = "I like apples.";
String searchText = "apples";
String replacementText = "oranges";
String replacedText;

void setup() {
  Serial.begin(9600);
  // 文本替换
  if (originalText.indexOf(searchText) != -1) {
    replacedText = originalText.substring(0, originalText.indexOf(searchText)) 
    + replacementText 
    + originalText.substring(originalText.indexOf(searchText) + searchText.length());
  } else {
    replacedText = originalText;
  }
  // 显示结果
  Serial.println(replacedText);
}

void loop() {
  // 程序未在loop()部分运行
}
```
输出结果：
```
I like oranges.
```

## Deep Dive (深入了解)
Arduino字符串替换的需求不如服务器或者大型程序那般常见，但在处理诸如传感器数据文字处理时往往很有帮助。历史上，文本处理在编程早期阶段起了重要作用，比如UNIX系统中的正则表达式利用以及工具如sed和awk。尽管Arduino核心库不支持这些高级功能，替换功能便是简化版的文本处理例子。你还可以写自定义函数或者使用更高级的库来达到类似sed的效果。

## See Also (另请参阅)
- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Arduino String indexOf() Documentation](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/indexof/)
- [Arduino String substring() Documentation](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Arduino String length() Documentation](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/length/)

该教程侧重于基础用法，希望你对Arduino中的搜索和文本替换方式有所了解。如果需要更高级的处理，可以考虑看看以上链接。

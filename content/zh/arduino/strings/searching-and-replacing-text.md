---
date: 2024-01-20 17:58:03.755266-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u8F93\u51FA\u7ED3\u679C\uFF1A\
  ."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.341481-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C"
weight: 10
---

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

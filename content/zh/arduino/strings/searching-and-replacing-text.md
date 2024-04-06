---
date: 2024-01-20 17:58:03.755266-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.254992-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) Arduino\u5B57\u7B26\u4E32\u66FF\u6362\u7684\
  \u9700\u6C42\u4E0D\u5982\u670D\u52A1\u5668\u6216\u8005\u5927\u578B\u7A0B\u5E8F\u90A3\
  \u822C\u5E38\u89C1\uFF0C\u4F46\u5728\u5904\u7406\u8BF8\u5982\u4F20\u611F\u5668\u6570\
  \u636E\u6587\u5B57\u5904\u7406\u65F6\u5F80\u5F80\u5F88\u6709\u5E2E\u52A9\u3002\u5386\
  \u53F2\u4E0A\uFF0C\u6587\u672C\u5904\u7406\u5728\u7F16\u7A0B\u65E9\u671F\u9636\u6BB5\
  \u8D77\u4E86\u91CD\u8981\u4F5C\u7528\uFF0C\u6BD4\u5982UNIX\u7CFB\u7EDF\u4E2D\u7684\
  \u6B63\u5219\u8868\u8FBE\u5F0F\u5229\u7528\u4EE5\u53CA\u5DE5\u5177\u5982sed\u548C\
  awk\u3002\u5C3D\u7BA1Arduino\u6838\u5FC3\u5E93\u4E0D\u652F\u6301\u8FD9\u4E9B\u9AD8\
  \u7EA7\u529F\u80FD\uFF0C\u66FF\u6362\u529F\u80FD\u4FBF\u662F\u7B80\u5316\u7248\u7684\
  \u6587\u672C\u5904\u7406\u4F8B\u5B50\u3002\u4F60\u8FD8\u53EF\u4EE5\u5199\u81EA\u5B9A\
  \u4E49\u51FD\u6570\u6216\u8005\u4F7F\u7528\u66F4\u9AD8\u7EA7\u7684\u5E93\u6765\u8FBE\
  \u5230\u7C7B\u4F3Csed\u7684\u6548\u679C\u3002"
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

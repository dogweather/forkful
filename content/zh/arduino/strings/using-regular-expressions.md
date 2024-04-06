---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:55.461296-07:00
description: "\u5982\u4F55\u5B9E\u73B0\uFF1A Arduino\u5728\u5176\u6807\u51C6\u5E93\
  \u4E2D\u6CA1\u6709\u76F4\u63A5\u652F\u6301regex\u3002\u4F46\u662F\uFF0C\u5BF9\u4E8E\
  \u7B80\u5355\u6A21\u5F0F\uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528\u57FA\u672C\u5B57\u7B26\
  \u4E32\u51FD\u6570\u5B9E\u73B0\u7C7B\u4F3Cregex\u7684\u529F\u80FD\uFF0C\u6216\u8005\
  \u5BF9\u4E8E\u66F4\u590D\u6742\u7684\u9700\u6C42\uFF0C\u96C6\u6210\u7B2C\u4E09\u65B9\
  \u5E93\uFF0C\u5982`regex`\u3002"
lastmod: '2024-04-05T22:38:47.206546-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u5B9E\u73B0\uFF1A Arduino\u5728\u5176\u6807\u51C6\u5E93\u4E2D\
  \u6CA1\u6709\u76F4\u63A5\u652F\u6301regex\u3002\u4F46\u662F\uFF0C\u5BF9\u4E8E\u7B80\
  \u5355\u6A21\u5F0F\uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528\u57FA\u672C\u5B57\u7B26\u4E32\
  \u51FD\u6570\u5B9E\u73B0\u7C7B\u4F3Cregex\u7684\u529F\u80FD\uFF0C\u6216\u8005\u5BF9\
  \u4E8E\u66F4\u590D\u6742\u7684\u9700\u6C42\uFF0C\u96C6\u6210\u7B2C\u4E09\u65B9\u5E93\
  \uFF0C\u5982`regex`\u3002"
title: "\u4F7F\u7528\u6B63\u5219\u8868\u8FBE\u5F0F"
weight: 11
---

## 如何实现：
Arduino在其标准库中没有直接支持regex。但是，对于简单模式，你可以使用基本字符串函数实现类似regex的功能，或者对于更复杂的需求，集成第三方库，如`regex`。

### 不使用Regex的基本字符串匹配
对于基础需求，如查找子字符串，你可以使用`String.indexOf()`函数：
```cpp
String data = "Sensor value: 12345";
int index = data.indexOf("value:");
if (index != -1) {
  String value = data.substring(index + 6).trim();
  Serial.println(value); // 输出：12345
}
```

### 使用第三方库实现Regex
为了处理更复杂的模式，你可能会考虑使用像`regex`这样的库。安装库后，可以按照以下方式使用：

1. **安装**：`regex`库可能在Arduino库管理器中不易找到，因此你可能需要从可信来源下载并将其添加至你的Arduino库文件夹中手动安装。

2. **示例用法**：
假设该库提供了类似标准regex实现的功能，你可以如下使用它：

```cpp
#include <regex.h>

void setup() {
  Serial.begin(9600);
  while (!Serial); // 等待Serial准备就绪
  
  regex_t reg;
  const char* pattern = "[0-9]+"; // 匹配一系列数字
  regcomp(&reg, pattern, REG_EXTENDED);
  
  const char* test_str = "Sensor value: 12345";
  
  regmatch_t matches[1];
  if (regexec(&reg, test_str, 1, matches, 0) == 0) {
    // 提取并打印匹配的部分
    int start = matches[0].rm_so;
    int end = matches[0].rm_eo;
    char match[end-start+1];
    strncpy(match, test_str + start, end-start);
    match[end-start] = '\0';
    
    Serial.print("发现匹配：");
    Serial.println(match); // 输出：12345
  } else {
    Serial.println("未找到匹配");
  }
  
  regfree(&reg); // 释放regex分配的内存
}

void loop() {
  // 将你的主要代码放在这里，以便重复运行：
}
```

**注意**：这里使用的语法和特定功能是为了说明目的，可能会根据你选择的`regex`库的实际实现细节而有所变化。始终参考库的文档以获取准确和最新的信息。

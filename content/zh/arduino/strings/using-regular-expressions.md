---
title:                "使用正则表达式"
aliases: - /zh/arduino/using-regular-expressions.md
date:                  2024-02-03T19:15:55.461296-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用正则表达式"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么及为什么？
正则表达式（regex）是定义搜索模式的字符序列，主要用于字符串匹配和操作。程序员在Arduino项目中利用regex来解析串行输入、验证用户输入或从字符串中提取数据，提高数据处理的效率和灵活性。

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

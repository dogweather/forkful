---
title:                "处理 YAML 文件"
html_title:           "Bash: 处理 YAML 文件"
simple_title:         "处理 YAML 文件"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
YAML是一种数据序列化格式，易于人类阅读与写作，同时也易于计算机解析。编程时使用YAML可简化配置文件、数据交换和存储等复杂任务。

## 如何：
```Arduino
// 导入Arduino YAML库
#include <ArduinoYAML.h>

void setup() {
  Serial.begin(9600);
  // 简单YAML例子
  const char *yaml = "title: Arduino编程\nversion: 1.0\nfeatures:\n  - 易于学习\n  - 灵活性高";
  
  // 解析YAML
  YamlParser<100> parser(yaml, strlen(yaml)); // 将字符串和长度传给解析器
  YamlNode root = parser.parse();

  // 输出YAML内容
  if (root.success()) {
    Serial.println(root["title"].as<char *>());  // 输出标题
    Serial.println(root["version"].as<float>()); // 输出版本
    YamlNode features = root["features"];
    
    for (int i = 0; i < features.size(); i++) {
      Serial.println(features[i].as<char *>());  // 输出特性列表
    }
  }
}

void loop() {
  // 无需循环逻辑
}
```
样例输出：
```
Arduino编程
1.0
易于学习
灵活性高
```

## 深入了解
YAML，"YAML Ain't Markup Language"（递归缩写，意为YAML不是标记语言），起源于2001年，用来取代复杂的XML。JSON是YAML的替代选项，更侧重于数据交换但不如YAML人类友好。Arduino对YAML的支持通常通过第三方库实现，例如ArduinoYAML库，它解析YAML格式，主要用于配置及数据表示。

## 参阅资料
- YAML官方网站：https://yaml.org
- ArduinoYAML库文档：https://github.com/arduino-libraries/ArduinoYAML
- YAML 1.2 规范：https://yaml.org/spec/1.2/spec.html
- Arduino官方论坛：https://forum.arduino.cc

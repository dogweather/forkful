---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:43.065440-07:00
description: "YAML\uFF08YAML Ain't Markup Language\uFF09\u662F\u4E00\u79CD\u4EBA\u7C7B\
  \u53EF\u8BFB\u7684\u6570\u636E\u5E8F\u5217\u5316\u6807\u51C6\uFF0C\u53EF\u4EE5\u7528\
  \u4E8E\u914D\u7F6E\u6587\u4EF6\u3001\u7A0B\u5E8F\u95F4\u901A\u4FE1\u548C\u6570\u636E\
  \u5B58\u50A8\u3002\u7A0B\u5E8F\u5458\u5728Arduino\u9879\u76EE\u4E2D\u4F7F\u7528\
  YAML\u4EE5\u7B80\u5316\u5E94\u7528\u7A0B\u5E8F\u7684\u914D\u7F6E\u8FC7\u7A0B\uFF0C\
  \u4F7F\u53C2\u6570\u4FEE\u6539\u66F4\u52A0\u5BB9\u6613\u800C\u4E0D\u9700\u8981\u6DF1\
  \u5165\u7814\u7A76\u4EE3\u7801\uFF0C\u63D0\u9AD8\u53EF\u8BFB\u6027\uFF0C\u5E76\u7B80\
  \u5316\u914D\u7F6E\u5171\u4EAB\u3002"
lastmod: '2024-03-11T00:14:21.885839-06:00'
model: gpt-4-0125-preview
summary: "YAML\uFF08YAML Ain't Markup Language\uFF09\u662F\u4E00\u79CD\u4EBA\u7C7B\
  \u53EF\u8BFB\u7684\u6570\u636E\u5E8F\u5217\u5316\u6807\u51C6\uFF0C\u53EF\u4EE5\u7528\
  \u4E8E\u914D\u7F6E\u6587\u4EF6\u3001\u7A0B\u5E8F\u95F4\u901A\u4FE1\u548C\u6570\u636E\
  \u5B58\u50A8\u3002\u7A0B\u5E8F\u5458\u5728Arduino\u9879\u76EE\u4E2D\u4F7F\u7528\
  YAML\u4EE5\u7B80\u5316\u5E94\u7528\u7A0B\u5E8F\u7684\u914D\u7F6E\u8FC7\u7A0B\uFF0C\
  \u4F7F\u53C2\u6570\u4FEE\u6539\u66F4\u52A0\u5BB9\u6613\u800C\u4E0D\u9700\u8981\u6DF1\
  \u5165\u7814\u7A76\u4EE3\u7801\uFF0C\u63D0\u9AD8\u53EF\u8BFB\u6027\uFF0C\u5E76\u7B80\
  \u5316\u914D\u7F6E\u5171\u4EAB\u3002"
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
---

{{< edit_this_page >}}

## 什么 & 为什么？

YAML（YAML Ain't Markup Language）是一种人类可读的数据序列化标准，可以用于配置文件、程序间通信和数据存储。程序员在Arduino项目中使用YAML以简化应用程序的配置过程，使参数修改更加容易而不需要深入研究代码，提高可读性，并简化配置共享。

## 如何操作：

直接在Arduino上处理YAML并不像在更高级的编程环境中那样直接，这是因为内存限制和缺少原生的YAML处理库。然而，对于需要YAML解析或生成的项目来说，一个典型的方法是使用伴侣计算机（如树莓派）或使用外部脚本将YAML文件转换为更适合Arduino的格式（如JSON）。为了演示，我们来关注后一种方法，使用一个流行库：ArduinoJson。

**步骤 1：** 将您的YAML配置转换为JSON。您可以使用在线工具或命令行实用程序如`yq`。

YAML文件（`config.yaml`）：
```yaml
wifi:
  ssid: "YourSSID"
  password: "YourPassword"
```

转换为JSON（`config.json`）：
```json
{
  "wifi": {
    "ssid": "YourSSID",
    "password": "YourPassword"
  }
}
```

**步骤 2：** 在您的Arduino草图中使用ArduinoJson库解析JSON文件。首先，您需要通过Arduino IDE的库管理器安装ArduinoJson库。

**步骤 3：** 在代码中加载并解析JSON。由于Arduino的存储限制，请想像JSON字符串存储在变量中或从SD卡读取。

示例Arduino草图：
```cpp
#include <ArduinoJson.h>

const char* jsonConfig = "{\"wifi\":{\"ssid\":\"YourSSID\",\"password\":\"YourPassword\"}}";

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc;
  DeserializationError error = deserializeJson(doc, jsonConfig);

  if (error) {
    Serial.print(F("deserializeJson() failed: "));
    Serial.println(error.f_str());
    return;
  }

  const char* ssid = doc["wifi"]["ssid"]; // "YourSSID"
  const char* password = doc["wifi"]["password"]; // "YourPassword"

  Serial.print("SSID: ");
  Serial.println(ssid);
  Serial.print("Password: ");
  Serial.println(password);
}

void loop() {
  // 在这个例子中这里没有内容
}
```

执行草图后的输出：
```
SSID: YourSSID
Password: YourPassword
```

这种方法，通过转换为JSON并利用ArduinoJson库，允许在Arduino项目中可管理地处理YAML配置，从而绕过了在微控制器上直接解析YAML。

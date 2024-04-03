---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:43.065440-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A\u2026"
lastmod: '2024-03-13T22:44:48.086391-06:00'
model: gpt-4-0125-preview
summary: "\u76F4\u63A5\u5728Arduino\u4E0A\u5904\u7406YAML\u5E76\u4E0D\u50CF\u5728\u66F4\
  \u9AD8\u7EA7\u7684\u7F16\u7A0B\u73AF\u5883\u4E2D\u90A3\u6837\u76F4\u63A5\uFF0C\u8FD9\
  \u662F\u56E0\u4E3A\u5185\u5B58\u9650\u5236\u548C\u7F3A\u5C11\u539F\u751F\u7684YAML\u5904\
  \u7406\u5E93\u3002\u7136\u800C\uFF0C\u5BF9\u4E8E\u9700\u8981YAML\u89E3\u6790\u6216\
  \u751F\u6210\u7684\u9879\u76EE\u6765\u8BF4\uFF0C\u4E00\u4E2A\u5178\u578B\u7684\u65B9\
  \u6CD5\u662F\u4F7F\u7528\u4F34\u4FA3\u8BA1\u7B97\u673A\uFF08\u5982\u6811\u8393\u6D3E\
  \uFF09\u6216\u4F7F\u7528\u5916\u90E8\u811A\u672C\u5C06YAML\u6587\u4EF6\u8F6C\u6362\
  \u4E3A\u66F4\u9002\u5408Arduino\u7684\u683C\u5F0F\uFF08\u5982JSON\uFF09\u3002\u4E3A\
  \u4E86\u6F14\u793A\uFF0C\u6211\u4EEC\u6765\u5173\u6CE8\u540E\u4E00\u79CD\u65B9\u6CD5\
  \uFF0C\u4F7F\u7528\u4E00\u4E2A\u6D41\u884C\u5E93\uFF1AArduinoJson."
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
weight: 41
---

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

---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:43.065440-07:00
description: ''
lastmod: '2024-04-05T22:38:47.242029-06:00'
model: gpt-4-0125-preview
summary: "**\u6B65\u9AA4 1\uFF1A** \u5C06\u60A8\u7684YAML\u914D\u7F6E\u8F6C\u6362\u4E3A\
  JSON\u3002\u60A8\u53EF\u4EE5\u4F7F\u7528\u5728\u7EBF\u5DE5\u5177\u6216\u547D\u4EE4\
  \u884C\u5B9E\u7528\u7A0B\u5E8F\u5982`yq`\u3002"
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

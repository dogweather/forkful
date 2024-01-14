---
title:                "Arduino: 使用json进行编程"
simple_title:         "使用json进行编程"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## 为什么

为什么要使用JSON？JSON（JavaScript Object Notation）是一种轻量级的数据交换格式，常用于在网络应用程序中传输数据。它易于阅读和编写，同时也易于解析和生成，因此在Arduino编程中也经常被使用。

## 如何操作

要在Arduino中使用JSON，首先需要安装一个JSON库，如ArduinoJson。然后，我们可以使用库中的函数来创建和解析JSON对象。下面是一个简单的示例，用于创建一个JSON对象，并将其输出到串行监视器中。

```Arduino
#include <ArduinoJson.h> //导入JSON库

void setup() {
    Serial.begin(9600); //初始化串行通信
    StaticJsonDocument<200> doc; //创建JSON对象，指定最大内存大小为200字节
    doc["name"] = "John"; //给JSON对象添加键值对
    doc["age"] = 25;
    doc["city"] = "Beijing";
    serializeJson(doc, Serial); //将JSON对象序列化为字符串并输出到串行监视器
}

void loop() {

}
```

输出结果如下所示：

``` 
{"name":"John","age":25,"city":"Beijing"}
```

## 深入了解

除了创建和解析基本的JSON对象，JSON库还提供了许多其他功能，如在JSON对象中添加数组和嵌套对象，以及从JSON文件中读取数据等。此外，我们也可以通过使用其他Arduino库来实现更复杂的操作，如通过HTTP请求获取JSON数据，并进行解析和处理。

## 参考链接

- ArduinoJson官方文档：https://arduinojson.org/
- 通过HTTP请求获取JSON数据教程：https://randomnerdtutorials.com/esp32-http-get-post-arduino-json/
- 使用ArduinoJson创建和解析嵌套JSON对象教程：https://lastminuteengineers.com/arduino-json-tutorial/
- 从文件中读取JSON数据教程：https://www.mischianti.org/2019/06/02/esp8266-nodemcu-post-get-json/
- 使用ArduinoJson处理动态JSON数据教程：https://techtutorialsx.com/2019/05/05/esp32-arduino-getting-dynamic-attributes-with-json-parsing/ 

## 参考链接
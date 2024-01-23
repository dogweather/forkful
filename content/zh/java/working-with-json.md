---
title:                "处理JSON数据"
html_title:           "Arduino: 处理JSON数据"
simple_title:         "处理JSON数据"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
使用JSON来存储和交换数据简单快捷。程序员喜欢它因为其语法清晰，且易于人类读写、机器解析。

## How to:
```Java
import org.json.JSONObject;

public class JSONExample {
    public static void main(String[] args) {
        // 创建一个 JSON 对象
        JSONObject obj = new JSONObject();
        obj.put("name", "张三");
        obj.put("age", 25);
        obj.put("isDeveloper", true);

        // 打印 JSON 对象
        System.out.println(obj.toString());

        // 从 JSON 对象读取数据
        String name = obj.getString("name");
        System.out.println("姓名: " + name);
    }
}
```
输出:
```
{"name":"张三","age":25,"isDeveloper":true}
姓名: 张三
```

## Deep Dive
JSON，即JavaScript对象表示法，是数据交换格式的一种标准，1999年由Douglas Crockford提出。主要对标的是XML，但因为更轻量级和易于解析，它成为了更受青睐的选择。Java原生没有JSON解析器，但有很多第三方库，比如`org.json`，`Google Gson`和`Jackson`。这些库各有特点，你可以根据需要选择合适的解析器。

## See Also
- JSON 官方网站以了解更多细节: [JSON](https://www.json.org/json-zh.html)
- `org.json` 库文档: [GitHub - stleary/JSON-java](https://github.com/stleary/JSON-java)
- Google Gson 用户指南: [Gson User Guide](https://github.com/google/gson/blob/master/UserGuide.md)
- Jackson 快速入门: [Jackson In Five Minutes](https://github.com/FasterXML/jackson-docs/wiki/JacksonInFiveMinutes)

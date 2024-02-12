---
title:                "使用JSON进行编程"
date:                  2024-02-03T19:23:10.400623-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用JSON进行编程"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？
使用 JSON (JavaScript 对象表示法) 意味着在 Java 应用程序中处理这种轻量级数据交换格式。程序员选择 JSON 来序列化和传输结构化数据，以及通过网络轻松配置和存储数据，因为它易于人类阅读和语言无关。

## 如何操作：
让我们卷起袖子，开始用 Java 进行 JSON 编码。

首先，你需要一个 JSON 处理库，如 `Jackson` 或 `Google Gson`。这里我们将使用 `Jackson`，所以请将此依赖项添加到你的 `pom.xml`：

```xml
<dependency>
    <groupId>com.fasterxml.jackson.core</groupId>
    <artifactId>jackson-databind</artifactId>
    <version>2.13.1</version>
</dependency>
```

现在，让我们序列化（写入）一个简单的 Java 对象到 JSON：

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = new Person("Alex", 30);
            String json = mapper.writeValueAsString(person);
            System.out.println(json);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

class Person {
    public String name;
    public int age;

    public Person(String name, int age) {
        this.name = name;
        this.age = age;
    }
}
```

输出应该是：

```json
{"name":"Alex","age":30}
```

现在，将 JSON 反序列化（读取）回 Java 对象：

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        String json = "{\"name\":\"Alex\",\"age\":30}";
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = mapper.readValue(json, Person.class);
            System.out.println(person.name + " 今年 " + person.age + " 岁。");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

输出将是：

```
Alex 今年 30 岁。
```

## 深入了解
JSON 的简单性和有效性已使其成为网络上数据交换的事实标准，推翻了 XML 的宝座。JSON 在 2000 年代初被引入，最初是从 JavaScript 派生出来的，但现在几乎所有语言都支持。

JSON 的替代品包括 XML，它更冗长，以及二进制格式，如 Protocol Buffers 或 MessagePack，它们的可读性较差但在大小和速度上更高效。每种都有其用例；选择取决于你的特定数据需求和上下文。

在 Java 中，除了 `Jackson` 和 `Gson`，我们还有 `JsonB` 和 `org.json` 等其他库来处理 JSON。Jackson 提供基于流的处理并且以速度著称，而 Gson 因其易用性而受到赞扬。JsonB 是 Jakarta EE 的一部分，提供了一种更标准化的方法。

在实现 JSON 时，记得正确处理你的异常 - 你的代码应该对不良输入有很强的鲁棒性。此外，考虑自动数据绑定的安全含义 - 始终验证你的输入！

## 参见
- [Jackson 项目](https://github.com/FasterXML/jackson)
- [Gson 项目](https://github.com/google/gson)
- [JSON 规范](https://www.json.org/json-en.html)
- [JsonB 规范](https://jakarta.ee/specifications/jsonb/)

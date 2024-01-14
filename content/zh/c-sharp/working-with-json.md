---
title:                "C#: 使用json进行编程"
simple_title:         "使用json进行编程"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

为什么：JSON 是一种轻量级的数据交换格式，可以在各种编程语言中快速地解析和生成数据。它也被广泛用于网络开发和移动应用程序。

如何处理：下面是一个简单的示例代码，演示如何使用 C# 对 JSON 数据进行解析和生成。

```C#
// 解析 JSON 字符串
string json = "{\"name\":\"John\",\"age\":30,\"city\":\"New York\"}";
JObject obj = JObject.Parse(json);
// 获取数据
string name = obj["name"].ToString();
int age = (int)obj["age"];
string city = obj["city"].ToString();
// 输出数据
Console.WriteLine("Name: {0}, Age: {1}, City: {2}", name, age, city);

// 生成 JSON 字符串
JObject person = JObject.FromObject(new
{
    name = "Jane",
    age = 25,
    city = "London"
});
// 输出结果
Console.WriteLine(person.ToString());
```

深入了解：除了上述基本操作，还可以使用 C# 中提供的更多方法来处理 JSON 数据。例如，可以使用 `JArray` 来解析和生成 JSON 数组，使用 `JToken` 来访问嵌套的 JSON 数据，使用 LINQ 查询来过滤和操作数据等等。

参考链接：

- [Newtonsoft.Json 文档](https://www.newtonsoft.com/json)
- [C# 中使用 Newtonsoft.Json 操作 JSON 数据](https://blog.csdn.net/onedance/article/details/79166540)

另请参阅：

- [使用 JSON 在网页应用中传输数据](https://www.w3schools.com/js/js_json_intro.asp)
- [Java 中处理 JSON 数据的方法](https://www.journaldev.com/2324/jackson-json-java-parser-api-example-tutorial)
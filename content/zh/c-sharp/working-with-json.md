---
title:                "处理JSON"
html_title:           "C#: 处理JSON"
simple_title:         "处理JSON"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## 为什么

在当今的软件开发领域，JSON（JavaScript对象表示法）已经成为一种非常流行的数据交换格式。它的简单性、易读性和跨平台支持使得它成为了开发人员的首选。通过使用JSON，我们可以轻松地在不同应用程序之间传递数据，如从后端服务器到前端网页。

## 如何操作

JSON在C#中的操作非常简单，下面我们将通过一些例子来展示如何使用C#来处理JSON数据。

首先，我们需要引用Newtonsoft.Json库，该库是C#中处理JSON的标准库。接着，我们使用`using`关键字将其导入到我们的代码中。

```C#
using Newtonsoft.Json;
```

接下来，我们定义一个JSON对象，并将其转换为字符串。

```C#
var person = new
{
    name = "John",
    age = 25,
    occupation = "Developer"
};

string json = JsonConvert.SerializeObject(person);
```

这里，我们定义了一个包含名字、年龄和职业的Person对象，并将其序列化为JSON字符串。接着，我们可以将这个字符串输出到控制台。

```C#
Console.WriteLine(json);
```

输出结果为：

```text
{"name":"John","age":25,"occupation":"Developer"}
```

接下来，我们将展示如何将JSON字符串反序列化为C#对象。

假设我们有一个包含人员信息的JSON字符串：

```text
string json = @"{
    'name': 'Jane',
    'age': 30,
    'occupation': 'Designer'
}";
```

首先，我们需要定义一个对应的C#类来表示此JSON对象。

```C#
public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
    public string Occupation { get; set; }
}
```

接着，我们可以通过调用`JsonConvert.DeserializeObject()`方法来将JSON字符串反序列化为对应的C#对象。

```C#
Person person = JsonConvert.DeserializeObject<Person>(json);
```

最后，我们可以访问这个对象的属性来获取相应的值。

```C#
Console.WriteLine(person.Name);
Console.WriteLine(person.Age);
Console.WriteLine(person.Occupation);
```

输出结果为：

```text
Jane
30
Designer
```

## 深入了解

除了上面提到的基本操作外，我们还可以通过C#来对JSON数据进行更加复杂的处理，例如：逐行读取JSON文件、使用LINQ（Language-Integrated Query）查询和筛选JSON数据等等。

此外，我们还可以使用一些第三方库来帮助我们更加方便地处理JSON数据，例如：Json.NET、FastJson等等。

## 参考资料

- [Json.NET文档](https://www.newtonsoft.com/json)
- [C#中的JSON操作指南](https://www.c-sharpcorner.com/article/a-guide-to-json-in-c-sharp/)
- [使用JSON.NET快速处理JSON数据](https://code.msdn.microsoft.com/windowsapps/Using-JSONNET-for-rapid-b242d6e9)
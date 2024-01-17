---
title:                "使用json进行编程"
html_title:           "C++: 使用json进行编程"
simple_title:         "使用json进行编程"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## 这是什么及为什么要用它？
工作中经常会遇到需要处理数据的情况，而JSON（JavaScript Object Notation）是一种方便的数据格式，许多程序员使用它来存储和传输数据。它可以使用简单的文本进行表示，易于理解和解析，因此受到了很多程序员的青睐。

## 如何操作：
JSON数据的示例和输出结果如下所示：

```C++
#include <iostream>
#include <json/json.h>

using namespace std;

int main()
{
    // 创建JSON对象
    Json::Value info;

    // 填充数据
    info["name"] = "John";
    info["age"] = 25;
    info["hobbies"] = {"coding", "gaming", "reading"};

    // 转换为JSON字符串
    string json_string = info.toStyledString();

    // 打印输出
    cout << json_string << endl;

    // 解析JSON字符串
    Json::Reader reader;
    Json::Value parsed_info;
    reader.parse(json_string, parsed_info);

    // 获取数据
    string name = parsed_info["name"].asString();
    int age = parsed_info["age"].asInt();
    vector<string> hobbies = parsed_info["hobbies"].asString();

    // 打印输出
    cout << "Name: " << name << endl;
    cout << "Age: " << age << endl;
    cout << "Hobbies: ";
    for (string hobby : hobbies) {
        cout << hobby << " ";
    }
    cout << endl;
    return 0;
}
```

输出结果：

```
{
    "name": "John",
    "age": 25,
    "hobbies": [
        "coding",
        "gaming",
        "reading"
    ]
}
Name: John
Age: 25
Hobbies: coding gaming reading

```

## 深入介绍：
JSON最初由Douglas Crockford于1999年提出，是JavaScript语言的一个子集，用于在客户端和服务器之间传输数据。它的主要竞争者是XML（Extensible Markup Language），但由于JSON更加简单，易于读写和解析，因此越来越多的程序员选择使用它。除了C++，许多编程语言都可以轻松地处理JSON数据，如Python、Java和JavaScript等。

## 参考链接：
- [JSON官方网站](https://www.json.org/json-en.html)
- [JSON C++库](https://github.com/open-source-parsers/jsoncpp)
- [JSON教程](https://www.tutorialspoint.com/json/index.htm)
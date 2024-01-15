---
title:                "读取文本文件"
html_title:           "C#: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么

有时，我们需要从外部文件中读取数据，这可以帮助我们更有效地处理大量数据而不是手动输入。在C#中，我们可以使用简单的代码来读取文本文件，让我们来看看如何做到这一点吧。

## 如何进行

在C#中，我们可以使用`System.IO`命名空间中的`StreamReader`类来读取文本文件。首先，让我们定义一个文件路径，并创建一个`StreamReader`对象来打开文件，并将文件内容读取到一个字符串中。

```C#
string filePath = "example.txt"; // 定义文件路径
StreamReader reader = new StreamReader(filePath); // 创建StreamReader对象
string fileContents = reader.ReadToEnd(); // 读取文件内容并存储到字符串中
```

现在，我们可以使用`fileContents`变量来访问我们需要的数据。例如，假设我们的文本文件内容如下：

```
Name: John Smith
Age: 25
Occupation: Software Engineer
```

我们可以使用`Split`方法将数据分割成不同的行，并使用`Substring`方法来提取每行中的数据。下面是一个例子，我们可以通过这种方式获取姓名和年龄。

```C#
string[] lines = fileContents.Split('\n'); // 分割文件内容为不同的行
string nameLine = lines[0]; // 获取姓名行
string ageLine = lines[1]; // 获取年龄行
string name = nameLine.Substring(6); // 提取姓名
int age = Convert.ToInt32(ageLine.Substring(5)); // 提取年龄并转换为整数
Console.WriteLine("Name: " + name); // 输出姓名
Console.WriteLine("Age: " + age); // 输出年龄
```

输出将会是：

```
Name: John Smith
Age: 25
```

## 深入了解

除了使用`Split`和`Substring`方法来处理文本文件外，我们还可以使用正则表达式来提取特定的数据。C#中有一个`Regex`类可以帮助我们进行正则表达式操作。下面是如何使用正则表达式来提取姓名和年龄的示例代码：

```C#
MatchCollection matches = Regex.Matches(fileContents, @"\b([A-Za-z]+:)\s*(\w+)"); // 使用正则表达式匹配数据
foreach (Match match in matches)
{
    string label = match.Groups[1].Value; // 获取标签
    string value = match.Groups[2].Value; // 获取值
    if (label == "Name:") // 如果标签是姓名
    {
        Console.WriteLine("Name: " + value); // 输出姓名
    }
    else if (label == "Age:") // 如果标签是年龄
    {
        Console.WriteLine("Age: " + value); // 输出年龄
    }
}
```

输出将会是同样的结果：

```
Name: John Smith
Age: 25
```

## 参考链接

- [C# StreamReader Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader)
- [C# String.Split Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.split)
- [C# String.Substring Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring)
- [C# Regex Class](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex)
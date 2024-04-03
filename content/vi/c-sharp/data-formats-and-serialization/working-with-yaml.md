---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:55.728932-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi YAML\
  \ trong C#, b\u1EA1n c\u1EA7n th\u01B0 vi\u1EC7n YamlDotNet. B\u1EA1n c\xF3 th\u1EC3\
  \ c\xE0i \u0111\u1EB7t n\xF3 qua NuGet: `Install-Package YamlDotNet`. Tr\u01B0\u1EDB\
  c h\u1EBFt, h\xE3y\u2026"
lastmod: '2024-03-13T22:44:36.684175-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi YAML trong C#, b\u1EA1n c\u1EA7n\
  \ th\u01B0 vi\u1EC7n YamlDotNet."
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
weight: 41
---

## Làm thế nào:
Để làm việc với YAML trong C#, bạn cần thư viện YamlDotNet. Bạn có thể cài đặt nó qua NuGet: `Install-Package YamlDotNet`.

Trước hết, hãy chuyển đổi một đối tượng thành một chuỗi YAML:

```C#
using System;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

public class Person {
    public string FirstName { get; set; }
    public string LastName { get; set; }
    public int Age { get; set; }
}

class Program {
    static void Main(string[] args) {
        var person = new Person {
            FirstName = "Jamie",
            LastName = "Smith",
            Age = 35
        };

        var serializer = new SerializerBuilder()
            .WithNamingConvention(CamelCaseNamingConvention.Instance)
            .Build();

        string yaml = serializer.Serialize(person);
        Console.WriteLine(yaml);
    }
}
```

Kết quả đầu ra:
```yaml
firstName: Jamie
lastName: Smith
age: 35
```

Tiếp theo, hãy đọc một tệp YAML và giải mã nó:

```C#
using System;
using System.IO;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

class Program {
    static void Main(string[] args) {
        var yaml = @"
firstName: Jamie
lastName: Smith
age: 35
";
        
        var deserializer = new DeserializerBuilder()
            .WithNamingConvention(CamelCaseNamingConvention.Instance)
            .Build();
        
        Person person = deserializer.Deserialize<Person>(yaml);
        
        Console.WriteLine($"Hello, {person.FirstName} {person.LastName}!");
    }
}

public class Person {
    public string FirstName { get; set; }
    public string LastName { get; set; }
    public int Age { get; set; }
}
```

Kết quả đầu ra:
```
Hello, Jamie Smith!
```

## Sâu hơn nữa
YAML, có nghĩa là "YAML Ain't Markup Language," được đề xuất lần đầu tiên vào năm 2001 để dễ đọc hơn so với XML. Nó được sử dụng rộng rãi trong DevOps cho các cấu hình ống dẫn CI/CD, như trong các tệp Docker Compose hoặc bản kê khai triển khai Kubernetes. JSON là một siêu tập của YAML, có nghĩa là các tệp JSON cũng là YAML hợp lệ. Về mặt triển khai, phân tích cú pháp YAML trong C# yêu cầu một thư viện như YamlDotNet vì không có hỗ trợ tự nhiên.

## Xem thêm
- [Kho GitHub YamlDotNet](https://github.com/aaubry/YamlDotNet)
- [Trang web chính thức của YAML](https://yaml.org)
- [Quy cách YAML](https://yaml.org/spec/1.2/spec.html)

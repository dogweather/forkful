---
title:                "Làm việc với YAML"
aliases:
- vi/c-sharp/working-with-yaml.md
date:                  2024-01-28T22:11:55.728932-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c-sharp/working-with-yaml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
YAML là một tiêu chuẩn serial hóa dữ liệu thân thiện với người dùng được sử dụng trong lập trình cho các tệp cấu hình, lưu trữ dữ liệu và hơn thế nữa. Lập trình viên sử dụng nó vì tính dễ đọc và đơn giản trong các ứng dụng và hệ thống phức tạp.

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

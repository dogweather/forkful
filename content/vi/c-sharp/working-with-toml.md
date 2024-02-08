---
title:                "Làm việc với TOML"
aliases:
- vi/c-sharp/working-with-toml.md
date:                  2024-01-28T22:11:05.715830-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c-sharp/working-with-toml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?
TOML là viết tắt của Tom's Obvious, Minimal Language, một định dạng tập tin cấu hình dễ đọc nhờ vào ngữ nghĩa rõ ràng của nó. Lập trình viên sử dụng nó cho các tập tin cấu hình, đơn giản hóa việc trao đổi dữ liệu giữa các hệ thống, và bởi vì nó cân bằng giữa khả năng đọc của con người và khả năng phân tích cú pháp của máy.

## Cách thực hiện:
Đầu tiên, cài đặt một bộ phân tích cú pháp TOML như `Tomlyn`. Sử dụng trình quản lý gói của bạn:

```csharp
dotnet add package Tomlyn
```

Tiếp theo, phân tích một tập tin TOML:

```csharp
using Tomlyn;
using Tomlyn.Model;
using System;

var tomlContent = @"
[owner]
name = 'Tom Preston-Werner'
dob = 1979-05-27T07:32:00Z";

var tomlTable = Toml.Parse(tomlContent).ToModel();

Console.WriteLine($"Chủ nhân: {tomlTable["owner"]["name"]}");
// Kết quả:
// Chủ nhân: Tom Preston-Werner
```

Bây giờ, tạo và viết TOML:

```csharp
using Tomlyn;
using Tomlyn.Syntax;
using System;
using System.IO;

var doc = new DocumentSyntax
{
    Tables =
    {
        new TableSyntax("owner")
        {
            Items =
            {
                { "name", "Tom Preston-Werner" },
                { "dob", "1979-05-27T07:32:00Z" }
            }
        }
    }
};

var tomlString = doc.ToString();
File.WriteAllText("config.toml", tomlString);
Console.WriteLine("TOML đã được viết vào config.toml");
// Kết quả:
// TOML đã được viết vào config.toml
```

## Sâu hơn:
TOML được tạo ra bởi Tom Preston-Werner, đồng sáng lập của GitHub, vào khoảng năm 2013 như một phản ứng trước những hạn chế của các định dạng hiện có như YAML và JSON trong cài đặt cấu hình. Nó được thiết kế đặc biệt cho cấu hình với một trọng tâm mạnh mẽ vào việc trở nên dễ hiểu và không mơ hồ.

Các định dạng cấu hình thay thế bao gồm YAML, JSON và XML. Tuy nhiên, TOML nổi bật vì sự thân thiện với con người, đặc biệt là cho các tập tin cấu hình nơi mà việc chỉnh sửa bằng tay là phổ biến. JSON, mặc dù phổ biến, nhưng kém dễ đọc cho các cấu hình phức tạp, và XML thì dài dòng. YAML, mặc dù tương tự về khả năng đọc, nhưng có thể trở nên phức tạp với việc sử dụng nhiều khoảng trống và có rủi ro an ninh với một số nội dung.

Về mặt triển khai, TOML tập trung vào việc ánh xạ sạch sẽ tới một bảng băm, làm cho việc trích xuất dữ liệu trở nên dễ dàng dự đoán. Với việc phát hành phiên bản 1.0.0, TOML đã củng cố đặc tả của mình, nâng cao sự ổn định và hỗ trợ công cụ.

## Xem thêm:
- Kho chính thức GitHub TOML & Đặc tả: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- Tomlyn, thư viện .NET: [github.com/xoofx/Tomlyn](https://github.com/xoofx/Tomlyn)

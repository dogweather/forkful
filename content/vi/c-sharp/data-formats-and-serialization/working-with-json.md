---
title:                "Làm việc với JSON"
aliases:
- /vi/c-sharp/working-with-json.md
date:                  2024-01-28T22:10:21.094472-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c-sharp/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Làm việc với JSON có nghĩa là phân tích cú pháp và tạo ra dữ liệu JSON (Notation đối tượng JavaScript) trong các ứng dụng của bạn. Lập trình viên làm điều này bởi vì JSON là một định dạng trao đổi dữ liệu dựa trên văn bản, nhẹ, dễ đọc và viết cho con người, và dễ dàng cho máy móc để phân tích cú pháp và tạo ra.

## Làm thế nào:

Trong C#, bạn có khả năng sẽ sử dụng không gian tên `System.Text.Json` để xử lý JSON. Giả sử bạn có một class đơn giản:

```C#
public class Gamer
{
    public string GamerTag { get; set; }
    public int HighScore { get; set; }
}
```

Để serialize đối tượng này sang JSON, làm như sau:

```C#
var gamer = new Gamer { GamerTag = "PlayerOne", HighScore = 9001 };
string jsonString = JsonSerializer.Serialize(gamer);
Console.WriteLine(jsonString);
```

Kết quả:
```
{"GamerTag":"PlayerOne","HighScore":9001}
```

Để deserialize từ JSON trở lại thành đối tượng:

```C#
string jsonString = "{\"GamerTag\":\"PlayerOne\",\"HighScore\":9001}";
Gamer gamer = JsonSerializer.Deserialize<Gamer>(jsonString);
Console.WriteLine($"GamerTag: {gamer.GamerTag}, HighScore: {gamer.HighScore}");
```

Kết quả:
```
GamerTag: PlayerOne, HighScore: 9001
```

## Nghiên cứu sâu

JSON đã trở thành định dạng dữ liệu được ưa chuộng từ đầu những năm 2000, thay thế XML do sự đơn giản của nó. Mặc dù `System.Text.Json` hiện là thư viện được ưa chuộng trong C# cho .NET Core và .NET 5+, thư viện `Newtonsoft.Json` đã là tiêu chuẩn de facto trong nhiều năm. `System.Text.Json` tập trung vào hiệu suất cao và cấp phát bộ nhớ thấp, nhưng `Newtonsoft.Json` vẫn có bộ tính năng rộng lớn hơn mà một số ứng dụng có thể yêu cầu.

## Xem thêm

- Microsoft Docs về `System.Text.Json`: https://docs.microsoft.com/dotnet/standard/serialization/system-text-json-overview
- Newtonsoft.Json (Json.NET): https://www.newtonsoft.com/json
- Đặc tả JSON: https://www.json.org/json-en.html

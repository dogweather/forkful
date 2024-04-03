---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:21.094472-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong C#, b\u1EA1n c\xF3 kh\u1EA3 n\u0103ng\
  \ s\u1EBD s\u1EED d\u1EE5ng kh\xF4ng gian t\xEAn `System.Text.Json` \u0111\u1EC3\
  \ x\u1EED l\xFD JSON. Gi\u1EA3 s\u1EED b\u1EA1n c\xF3 m\u1ED9t class \u0111\u01A1\
  n gi\u1EA3n."
lastmod: '2024-03-13T22:44:36.685395-06:00'
model: gpt-4-0125-preview
summary: "Trong C#, b\u1EA1n c\xF3 kh\u1EA3 n\u0103ng s\u1EBD s\u1EED d\u1EE5ng kh\xF4\
  ng gian t\xEAn `System.Text.Json` \u0111\u1EC3 x\u1EED l\xFD JSON."
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
weight: 38
---

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

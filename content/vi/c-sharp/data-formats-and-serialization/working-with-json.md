---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:21.094472-07:00
description: "L\xE0m vi\u1EC7c v\u1EDBi JSON c\xF3 ngh\u0129a l\xE0 ph\xE2n t\xED\
  ch c\xFA ph\xE1p v\xE0 t\u1EA1o ra d\u1EEF li\u1EC7u JSON (Notation \u0111\u1ED1\
  i t\u01B0\u1EE3ng JavaScript) trong c\xE1c \u1EE9ng d\u1EE5ng c\u1EE7a b\u1EA1n.\
  \ L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u\u2026"
lastmod: '2024-02-25T18:49:35.028695-07:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi JSON c\xF3 ngh\u0129a l\xE0 ph\xE2n t\xEDch c\xFA\
  \ ph\xE1p v\xE0 t\u1EA1o ra d\u1EEF li\u1EC7u JSON (Notation \u0111\u1ED1i t\u01B0\
  \u1EE3ng JavaScript) trong c\xE1c \u1EE9ng d\u1EE5ng c\u1EE7a b\u1EA1n. L\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
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

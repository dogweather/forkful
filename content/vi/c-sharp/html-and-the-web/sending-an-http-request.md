---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:49.550792-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: C# l\xE0m cho vi\u1EC7c g\u1EEDi y\xEA\
  u c\u1EA7u HTTP tr\u1EDF n\xEAn \u0111\u01A1n gi\u1EA3n v\u1EDBi `HttpClient`. D\u01B0\
  \u1EDBi \u0111\xE2y l\xE0 b\u1ED9 x\u01B0\u01A1ng c\u1EE7a m\u1ED9t y\xEAu c\u1EA7\
  u GET."
lastmod: '2024-03-13T22:44:36.653367-06:00'
model: gpt-4-0125-preview
summary: "C# l\xE0m cho vi\u1EC7c g\u1EEDi y\xEAu c\u1EA7u HTTP tr\u1EDF n\xEAn \u0111\
  \u01A1n gi\u1EA3n v\u1EDBi `HttpClient`."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
weight: 44
---

## Cách thực hiện:
C# làm cho việc gửi yêu cầu HTTP trở nên đơn giản với `HttpClient`. Dưới đây là bộ xương của một yêu cầu GET:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        using HttpClient client = new HttpClient();
        HttpResponseMessage response = await client.GetAsync("http://example.com");
        response.EnsureSuccessStatusCode();
        string responseBody = await response.Content.ReadAsStringAsync();
        
        Console.WriteLine(responseBody);
    }
}
```

Mẫu đầu ra (được rút gọn):
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## Thảo luận sâu hơn
`HttpClient` được giới thiệu trong .NET Framework 4.5 để làm cho giao tiếp HTTP trở nên dễ dàng hơn. Trước đó, bạn có lẽ phải vật lộn với các lớp `HttpWebRequest` và `HttpWebResponse`, chúng phức tạp hơn.

Có các cách khác để gửi yêu cầu HTTP trong C#. `RestSharp` và `Flurl` là hai thư viện bên thứ ba phổ biến, cung cấp một giao diện trực quan hơn và các tính năng bổ sung. Nhưng `HttpClient` thường đủ đáp ứng cho hầu hết các nhu cầu.

Về mặt triển khai, `HttpClient` được thiết kế để tái sử dụng cho nhiều yêu cầu. Khởi tạo nó cho mỗi yêu cầu có thể làm cạn kiệt số lượng socket có sẵn dưới gánh nặng lớn. Luôn, và tôi muốn nói là luôn, chú ý đến việc xử lý đúng cách đối tượng `HttpClient` để tránh rò rỉ tài nguyên.

## Xem thêm
- Tài liệu `HttpClient` của Microsoft: [https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- Best practices của HttpClient: [https://aspnetmonsters.com/2016/08/2016-08-27-httpclientwrong/](https://aspnetmonsters.com/2016/08/2016-08-27-httpclientwrong/)
- Tương tác với RESTful API bằng `RestSharp`: [http://restsharp.org/](http://restsharp.org/)
- HTTP trôi chảy (HTTP được làm trôi chảy) với `Flurl`: [https://flurl.dev/](https://flurl.dev/)

---
aliases:
- /vi/c-sharp/sending-an-http-request/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:49.550792-07:00
description: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP l\xE0 c\xE1ch \u0111\u1EC3 c\xE1\
  c ch\u01B0\u01A1ng tr\xECnh giao ti\u1EBFp qua m\u1EA1ng, y\xEAu c\u1EA7u d\u1EEF\
  \ li\u1EC7u ho\u1EB7c g\u1EEDi d\u1EEF li\u1EC7u. L\u1EADp tr\xECnh vi\xEAn th\u1EF1\
  c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 t\u01B0\u01A1ng t\xE1c v\u1EDBi\u2026"
lastmod: 2024-02-18 23:08:50.694802
model: gpt-4-0125-preview
summary: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP l\xE0 c\xE1ch \u0111\u1EC3 c\xE1\
  c ch\u01B0\u01A1ng tr\xECnh giao ti\u1EBFp qua m\u1EA1ng, y\xEAu c\u1EA7u d\u1EEF\
  \ li\u1EC7u ho\u1EB7c g\u1EEDi d\u1EEF li\u1EC7u. L\u1EADp tr\xECnh vi\xEAn th\u1EF1\
  c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 t\u01B0\u01A1ng t\xE1c v\u1EDBi\u2026"
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
---

{{< edit_this_page >}}

## Gì & Tại sao?
Gửi một yêu cầu HTTP là cách để các chương trình giao tiếp qua mạng, yêu cầu dữ liệu hoặc gửi dữ liệu. Lập trình viên thực hiện điều này để tương tác với các API, dịch vụ hoặc để thu thập nội dung web.

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

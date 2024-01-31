---
title:                "Gửi một yêu cầu HTTP"
date:                  2024-01-28T22:07:49.550792-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi một yêu cầu HTTP"

category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c-sharp/sending-an-http-request.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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

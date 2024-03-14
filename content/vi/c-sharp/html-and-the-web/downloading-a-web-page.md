---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:23.929652-07:00
description: "T\u1EA3i m\u1ED9t trang web ngh\u0129a l\xE0 l\u1EA5y n\u1ED9i dung\
  \ HTML th\xF4 t\u1EEB internet b\u1EB1ng c\xE1ch s\u1EED d\u1EE5ng m\xE3 l\u1EAD\
  p tr\xECnh. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u\
  \ n\xE0y \u0111\u1EC3 x\u1EED l\xFD d\u1EEF li\u1EC7u, t\u01B0\u01A1ng\u2026"
lastmod: '2024-03-13T22:44:36.655835-06:00'
model: gpt-4-0125-preview
summary: "T\u1EA3i m\u1ED9t trang web ngh\u0129a l\xE0 l\u1EA5y n\u1ED9i dung HTML\
  \ th\xF4 t\u1EEB internet b\u1EB1ng c\xE1ch s\u1EED d\u1EE5ng m\xE3 l\u1EADp tr\xEC\
  nh. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\
  \u1EC3 x\u1EED l\xFD d\u1EEF li\u1EC7u, t\u01B0\u01A1ng\u2026"
title: "T\u1EA3i trang web"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Tải một trang web nghĩa là lấy nội dung HTML thô từ internet bằng cách sử dụng mã lập trình. Các lập trình viên thực hiện điều này để xử lý dữ liệu, tương tác với các dịch vụ web, hoặc đơn giản là lưu thông tin để sử dụng ngoại tuyến.

## Làm thế nào:

C# làm cho việc tải một trang web trở nên đơn giản với lớp `HttpClient`. Dưới đây là một ví dụ nhanh:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        using (HttpClient client = new HttpClient())
        {
            try
            {
                string url = "http://example.com"; // Thay đổi bằng URL mong muốn
                HttpResponseMessage response = await client.GetAsync(url);
                response.EnsureSuccessStatusCode();
                string responseBody = await response.Content.ReadAsStringAsync();
                
                Console.WriteLine(responseBody); // Xuất ra nội dung HTML thô
            }
            catch (HttpRequestException e)
            {
                Console.WriteLine("\nNgoại lệ bị bắt!");
                Console.WriteLine("Tin nhắn: {0} ", e.Message);
            }
        }
    }
}
```

Điều này sẽ xuất ra nội dung HTML của trang web được chỉ định vào bảng điều khiển.

## Sâu hơn

Trước khi có `HttpClient`, C# sử dụng các lớp như `WebClient` và `HttpWebRequest` để tải nội dung web. `HttpClient` là lớp mới nhất và được thiết kế để có thể tái sử dụng, hiệu quả và hỗ trợ các thao tác bất đồng bộ, làm cho nó trở thành lựa chọn ưu tiên cho các ứng dụng mới.

Có các lựa chọn thay thế. Ví dụ, thư viện bên thứ ba như `HtmlAgilityPack` có thể phân tích cú pháp HTML, làm cho việc điều hướng DOM hoặc trích xuất các phần cụ thể của thông tin trở nên dễ dàng hơn mà không cần phải xử lý với chuỗi HTML thô.

Khi tải các trang web, hãy nhớ: tôn trọng các tập tin robots.txt, xử lý ngoại lệ, và lưu ý đến các điều khoản sử dụng của các trang web.

## Xem thêm

- [Tài liệu Lớp HttpClient](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [Async và Await](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/async/)
- [HTML Agility Pack trên GitHub](https://github.com/zzzprojects/html-agility-pack)
- [Tôn trọng robots.txt](https://developers.google.com/search/docs/advanced/robots/intro)

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:14.201876-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Ch\xFAng ta h\xE3y b\u1EAFt \u0111\u1EA7\
  u ngay v\u1EDBi m\u1ED9t s\u1ED1 m\xE3. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9\
  t v\xED d\u1EE5 t\u1ED1i thi\u1EC3u s\u1EED d\u1EE5ng C# \u0111\u1EC3 g\u1EEDi m\u1ED9\
  t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3n."
lastmod: '2024-03-13T22:44:36.657113-06:00'
model: gpt-4-0125-preview
summary: "Ch\xFAng ta h\xE3y b\u1EAFt \u0111\u1EA7u ngay v\u1EDBi m\u1ED9t s\u1ED1\
  \ m\xE3."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3\
  n"
weight: 45
---

## Cách thực hiện:
Chúng ta hãy bắt đầu ngay với một số mã. Dưới đây là một ví dụ tối thiểu sử dụng C# để gửi một yêu cầu HTTP với xác thực cơ bản:

```C#
using System;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;

class Program
{
    static async Task Main()
    {
        using (var client = new HttpClient())
        {
            var credentials = Convert.ToBase64String(Encoding.ASCII.GetBytes("username:password"));
            client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", credentials);

            HttpResponseMessage response = await client.GetAsync("http://yourapi.com/protected");

            if (response.IsSuccessStatusCode)
            {
                string responseBody = await response.Content.ReadAsStringAsync();
                Console.WriteLine(responseBody);
            }
            else
            {
                Console.WriteLine($"Lỗi: {response.StatusCode}");
            }
        }
    }
}
```
Chạy cái này, và nếu điểm cuối và thông tin xác thực của bạn đúng, bạn sẽ nhận được tài nguyên. Nếu không, bạn sẽ thấy mã trạng thái lỗi.

## Tìm Hiểu Sâu Hơn
Xác thực cơ bản cũ, cực kỳ cũ, có từ những ngày đầu của Internet. Nó đơn giản: mã hóa base64 "username:password" và gắn nó vào tiêu đề `Authorization`.

Có các lựa chọn thay thế với tính bảo mật cao hơn: OAuth2, khóa API, hoặc mã token JWT. Xác thực cơ bản vẫn tồn tại do tính đơn giản của nó, nhưng hãy cảnh giác vì nó không được mã hóa và có thể bị chặn nếu không được sử dụng qua HTTPS.

Khi bạn sử dụng phương pháp này, hãy nhớ:
- Luôn sử dụng HTTPS để bảo vệ thông tin đăng nhập trong quá trình truyền tải.
- Nó hơi giống như để chìa khóa nhà dưới tấm thảm – tiện lợi nhưng dễ bị tổn thương. Vì vậy, hãy sử dụng nó cho các tình huống rủi ro thấp.
- Vì thông tin đăng nhập được truyền đi với mỗi yêu cầu, nó không phải là phương pháp hiệu quả nhất cho các hệ thống bận rộn.

## Xem Thêm
- [Tài liệu về Lớp HttpClient của Microsoft](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [Giải thích về Xác thực Cơ bản của Mozilla](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Bảng Cheat Sheet Xác thực của OWASP](https://owasp.org/www-project-cheat-sheets/cheatsheets/Authentication_Cheat_Sheet.html)

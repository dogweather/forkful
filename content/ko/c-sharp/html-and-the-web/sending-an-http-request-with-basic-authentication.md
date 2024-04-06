---
date: 2024-01-20 18:01:28.995929-07:00
description: "How to: (\uBC29\uBC95) C#\uC5D0\uC11C \uAE30\uBCF8 \uC778\uC99D\uACFC\
  \ \uD568\uAED8 HTTP \uC694\uCCAD\uC744 \uBCF4\uB0B4\uB824\uBA74 `HttpClient`\uB97C\
  \ \uC0AC\uC6A9\uD558\uACE0 \uD5E4\uB354\uC5D0 \uC778\uC99D \uC815\uBCF4\uB97C \uCD94\
  \uAC00\uD569\uB2C8\uB2E4. \uC544\uB798\uB294 \uAC04\uB2E8\uD55C \uC608\uC81C \uCF54\
  \uB4DC\uC785\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.961025-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) C#\uC5D0\uC11C \uAE30\uBCF8 \uC778\uC99D\uACFC \uD568\uAED8\
  \ HTTP \uC694\uCCAD\uC744 \uBCF4\uB0B4\uB824\uBA74 `HttpClient`\uB97C \uC0AC\uC6A9\
  \uD558\uACE0 \uD5E4\uB354\uC5D0 \uC778\uC99D \uC815\uBCF4\uB97C \uCD94\uAC00\uD569\
  \uB2C8\uB2E4."
title: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD55C HTTP \uC694\uCCAD \uBCF4\
  \uB0B4\uAE30"
weight: 45
---

## How to: (방법)
C#에서 기본 인증과 함께 HTTP 요청을 보내려면 `HttpClient`를 사용하고 헤더에 인증 정보를 추가합니다. 아래는 간단한 예제 코드입니다.

```C#
using System;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

class Program
{
    static async Task Main()
    {
        using var client = new HttpClient();
        var credentials = Convert.ToBase64String(Encoding.ASCII.GetBytes("username:password"));
        client.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Basic", credentials);

        try
        {
            var response = await client.GetAsync("http://yourapi.com/data");
            response.EnsureSuccessStatusCode();
            string responseBody = await response.Content.ReadAsStringAsync();
            Console.WriteLine(responseBody);
        }
        catch (HttpRequestException e)
        {
            Console.WriteLine("\nException Caught!");
            Console.WriteLine("Message :{0} ", e.Message);
        }
    }
}
```

이 코드는 "username"과 "password"를 사용하여 기본 인증을 수행하는 HttpClient를 생성하고 HTTP GET 요청을 보냅니다. 성공하면 응답 본문을 콘솔에 출력합니다.

## Deep Dive (깊이 있는 탐구)
기본 인증(Basic Authentication)은 HTTP에서 가장 오래된 인증 체계 중 하나입니다. 간단하지만, 사용자 이름과 비밀번호가 Base64로 인코딩되어 전송되기 때문에 HTTPS와 함께 사용하도록 권장됩니다. 대안으로 `OAuth`와 같은 보다 복잡한 인증 체계가 있으며, 토큰 기반 인증을 제공합니다. C#에서 이 기능을 구현할 때 `HttpClient` 클래스는 재사용이 가능해서 여러 요청에 대해 같은 인스턴스를 사용하는 것이 좋습니다. 또한, 응답 확인과 예외 처리를 통해 안정적인 구현을 할 수 있습니다.

## See Also (관련 자료)
- [HttpClient Class Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=net-6.0)
- [Basic Authentication Overview](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [OAuth 2.0](https://oauth.net/2/)

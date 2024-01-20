---
title:                "기본 인증을 이용한 HTTP 요청 보내기"
html_title:           "Arduino: 기본 인증을 이용한 HTTP 요청 보내기"
simple_title:         "기본 인증을 이용한 HTTP 요청 보내기"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇이고 왜? (What & Why?)

HTTP 요청을 전송하면서 기본 인증을 사용하는 것은, 인증된 사용자만이 수행하도록 보장하기 위한 것입니다. 개발자들은 약간의 보안 체계를 확고하게 하기 위해 이 방법을 사용합니다.

## 어떻게 하나요? (How to)

아래는 HTTP 요청에 기본 인증을 추가하는 C# 코드의 예제입니다:

```C#
using System;
using System.Net.Http;

class Program
{
    async static void Main()
    {
        var httpClient = new HttpClient();
    
        var byteArray = System.Text.Encoding.ASCII.GetBytes("username:password");
        httpClient.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Basic", Convert.ToBase64String(byteArray));
    
        HttpResponseMessage response = await httpClient.GetAsync("http://mysite.com/");
        
        Console.WriteLine("Response: " + await response.Content.ReadAsStringAsync());
     }
}
```

이 코드는 `username`과 `password`를 사용하여 `http://mysite.com/`에 기본 인증을 포함한 HTTP 요청을 보냅니다. 응답은 콘솔에 출력됩니다.

## 깊은 곳으로 (Deep Dive)

기본 인증이란 간단하게 사용자 이름과 비밀번호을 Base64 인코딩하여 HTTP 헤더에 포함시키는 것을 의미합니다. 이 방법은 1996년 HTTP/1.0에 최초로 도입되었습니다.

하지만, 기본 인증은 요청이 될 때마다 사용자 이름과 비밀번호를 매번 보내야 하기 때문에 보안에 취약합니다. 그렇기 때문에, 현대의 웹에서는 HTTPs와 결합하거나 보다 안전한 OAuth를 사용하는 것이 일반적입니다.

C#에서는 `HttpClient` 객체와 `AuthenticationHeaderValue` 클래스를 사용하여 기본 인증을 편리하게 적용할 수 있습니다.

## 참고자료 (See Also)

다음 링크들은 기본 인증과 관련된 C# 프로그래밍에 대해 더 자세한 정보를 제공합니다:

1. [HttpClient 클래스 (Microsoft 공식 문서)](https://docs.microsoft.com/ko-kr/dotnet/api/system.net.http.httpclient)
2. [HTTP Basic Access Authentication (Wikipedia)](https://en.wikipedia.org/wiki/Basic_access_authentication)
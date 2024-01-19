---
title:                "HTTP 요청 보내기"
html_title:           "Clojure: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

HTTP 요청을 보내는 것은 인터넷에서 데이터를 교환하는 방법입니다. 프로그래머들이 이를 사용하여 웹 서버와 상호작용하며, 웹페이지 또는 API에서 정보를 조회하거나 업데이트합니다.

## 어떻게:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static readonly HttpClient client = new HttpClient();

    static async Task Main()
    {
        HttpResponseMessage response = await client.GetAsync("http://example.com");
        response.EnsureSuccessStatusCode();
        string responseBody = await response.Content.ReadAsStringAsync();
        Console.WriteLine(responseBody);
    }
}
```

이 예제는 "http://example.com"에서 HTTP GET 요청을 보내고 응답을 출력합니다. 출력은 가져온 웹페이지의 HTML 소스입니다.

## 깊이 들어가기:

HTTP 요청을 보내는 것은 1990년 초 웹의 출현 이후로 이루어지고 있습니다. 이 기술은 웹 브라우저에서 웹 서버로 정보를 보낼 때 주로 사용됩니다. `HttpClient` 클래스는 .NET 프로그래밍에서 HTTP 통신을 수행하는 중심적인 방식입니다.

대안으로는 레거시 `WebRequest`나 `WebClient` 클래스를 사용할 수 있습니다. 그러나 `HttpClient`가 더 현대적이고 유연한 API를 제공하므로 일반적으로 이를 선호합니다.

`HttpClient.GetAsync` 메소드를 사용하여 GET 요청을 보낼 수 있습니다. POST, PUT, DELETE 등의 다른 HTTP 메소드를 위한 메소드도 있습니다.

## 참고 링크:

1. Microsoft Docs HttpClient 클래스: https://docs.microsoft.com/ko-kr/dotnet/api/system.net.http.httpclient
2. HTTP 요청 및 응답: https://developer.mozilla.org/ko/docs/Web/HTTP/Overview
3. `HttpClient`를 이용한 REST 호출: https://docs.microsoft.com/ko-kr/dotnet/csharp/tutorials/console-webapiclient
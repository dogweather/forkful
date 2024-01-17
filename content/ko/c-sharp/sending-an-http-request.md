---
title:                "HTTP 요청 보내기"
html_title:           "C#: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
HTTP 요청을 보내는 것은 웹 개발자들이 서버와의 통신을 하기 위해 사용하는 방법입니다. 이를 통해 웹 애플리케이션에서 데이터를 가져오거나 변경할 수 있습니다.

## 방법:
다음은 C#에서 HTTP 요청을 보내는 방법의 간단한 예제입니다:

```C#
using System;
using System.Net.Http;

class Program
{
    static async Task Main()
    {
        using (HttpClient client = new HttpClient())
        {
            HttpResponseMessage response = await client.GetAsync("https://example.com");
            string responseBody = await response.Content.ReadAsStringAsync();
            Console.WriteLine(responseBody);
        }
    }
}
```

위 코드는 `HttpClient` 클래스를 사용하여 `example.com`에서 GET 요청을 보내고 응답 본문을 출력하는 예제입니다.

## 깊이 들어가보기:
HTTP 요청은 1990년대 중반 월드 와이드 웹에서 정립되었습니다. 현재는 여러 가지 다른 프로토콜을 사용하여 서버와 통신하는 것도 가능합니다. 예를 들어, TCP 소켓을 사용하여 데이터를 전송할 수도 있습니다. 또한 `HttpClient` 대신 `HttpWebRequest` 클래스를 사용하여 훨씬 더 세부적인 제어를 할 수도 있습니다. 

## 관련 자료:
- [MDN - HTTP 요청](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)
- [Microsoft Docs - HttpClient 클래스](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [Microsoft Docs - HttpWebRequest 클래스](https://docs.microsoft.com/en-us/dotnet/api/system.net.httpwebrequest)
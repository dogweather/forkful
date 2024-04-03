---
date: 2024-01-20 17:59:20.687201-07:00
description: "How to: (\uBC29\uBC95) C#\uC5D0\uC11C HTTP \uC694\uCCAD\uC744 \uBCF4\
  \uB0BC \uB54C\uB294 `HttpClient` \uD074\uB798\uC2A4\uB97C \uC8FC\uB85C \uC0AC\uC6A9\
  \uD569\uB2C8\uB2E4. \uC544\uB798\uB294 \uAC04\uB2E8\uD558\uAC8C GET \uC694\uCCAD\
  \uC744 \uBCF4\uB0B4\uB294 \uC608\uC81C\uCF54\uB4DC\uC640 \uCD9C\uB825 \uACB0\uACFC\
  \uC785\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.229212-06:00'
model: gpt-4-1106-preview
summary: "C#\uC5D0\uC11C HTTP \uC694\uCCAD\uC744 \uBCF4\uB0BC \uB54C\uB294 `HttpClient`\
  \ \uD074\uB798\uC2A4\uB97C \uC8FC\uB85C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "HTTP \uC694\uCCAD \uBCF4\uB0B4\uAE30"
weight: 44
---

## How to: (방법)
C#에서 HTTP 요청을 보낼 때는 `HttpClient` 클래스를 주로 사용합니다. 아래는 간단하게 GET 요청을 보내는 예제코드와 출력 결과입니다.

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
            HttpResponseMessage response = await client.GetAsync("http://example.com");
            string responseBody = await response.Content.ReadAsStringAsync();

            Console.WriteLine(responseBody);
        }
    }
}
```

출력 결과는 http://example.com에서 제공하는 HTML 내용일 것입니다.

## Deep Dive (심층 분석)
과거에는 `WebRequest`나 `WebClient` 같은 클래스를 사용했지만, 지금은 `HttpClient`가 선호됩니다. 이는 비동기 작업을 위해 설계되었고, 성능도 좋습니다. `HttpClient`는 GET, POST, PUT, DELETE 등 다양한 HTTP 메소드를 지원합니다. 그리고, `HttpRequestMessage`와 `HttpResponseMessage`를 사용하여 요청과 응답을 더 세밀하게 제어할 수 있습니다. 

HttpWebRequest 대신 HttpClient를 사용하는 이유:
- 더 단순한 API
- 자동으로 연결 재사용
- 비동기 프로그래밍에 최적화

다른 HTTP 도구와 비교했을 때, `HttpClient`는 사용하기 쉬우면서도 강력한 성능을 제공합니다. 여러 `DelegatingHandler`를 이용해서 로깅, 인증, 오류 처리 등을 커스텀 할 수 있습니다.

## See Also (참고 자료)
- [HttpClient 클래스 문서](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [공식 Microsoft 가이드: HttpClient 사용하기](https://docs.microsoft.com/en-us/dotnet/csharp/tutorials/console-webapiclient)
- [HTTP 요청 방법들](https://www.restapitutorial.com/lessons/httpmethods.html)

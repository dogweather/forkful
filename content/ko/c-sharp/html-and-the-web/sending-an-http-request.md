---
title:                "HTTP 요청 보내기"
date:                  2024-01-20T17:59:20.687201-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP 요청 보내기"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

HTTP 요청 보내기는 서버에 데이터를 요청하거나 제출하는 과정입니다. 프로그래머들은 API와의 통신, 웹 컨텐츠 가져오기, 데이터 업로드 등을 위해 이를 사용합니다.

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

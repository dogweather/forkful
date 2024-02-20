---
date: 2024-01-20 17:59:20.687201-07:00
description: "HTTP \uC694\uCCAD \uBCF4\uB0B4\uAE30\uB294 \uC11C\uBC84\uC5D0 \uB370\
  \uC774\uD130\uB97C \uC694\uCCAD\uD558\uAC70\uB098 \uC81C\uCD9C\uD558\uB294 \uACFC\
  \uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 API\uC640\uC758\
  \ \uD1B5\uC2E0, \uC6F9 \uCEE8\uD150\uCE20 \uAC00\uC838\uC624\uAE30, \uB370\uC774\
  \uD130 \uC5C5\uB85C\uB4DC \uB4F1\uC744 \uC704\uD574 \uC774\uB97C \uC0AC\uC6A9\uD569\
  \uB2C8\uB2E4."
isCJKLanguage: true
lastmod: 2024-02-19 22:05:14.140335
model: gpt-4-1106-preview
summary: "HTTP \uC694\uCCAD \uBCF4\uB0B4\uAE30\uB294 \uC11C\uBC84\uC5D0 \uB370\uC774\
  \uD130\uB97C \uC694\uCCAD\uD558\uAC70\uB098 \uC81C\uCD9C\uD558\uB294 \uACFC\uC815\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 API\uC640\uC758 \uD1B5\
  \uC2E0, \uC6F9 \uCEE8\uD150\uCE20 \uAC00\uC838\uC624\uAE30, \uB370\uC774\uD130 \uC5C5\
  \uB85C\uB4DC \uB4F1\uC744 \uC704\uD574 \uC774\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4\
  ."
title: "HTTP \uC694\uCCAD \uBCF4\uB0B4\uAE30"
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

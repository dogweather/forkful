---
title:                "웹 페이지 다운로드하기"
html_title:           "Bash: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# 웹 페이지 다운로드를 위한 C# 사용하기 

## 무엇 & 왜?

웹 페이지 다운로드는 인터넷의 특정 웹 사이트 내용을 컴퓨터에 저장하는 것을 의미합니다. 이를 통해 프로그래머들은 웹 사이트의 데이터를 분석하거나, 오프라인으로 이용할 수 있게 합니다.

## 어떻게 하나요:

C#에서는 `HttpClient` 클래스를 통해 간단하게 웹 페이지의 내용을 다운로드 받을 수 있습니다.

```C#
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static readonly HttpClient client = new HttpClient();

    static async Task Main()
    {
        try
        {
            HttpResponseMessage response = await client.GetAsync("http://example.com");
            response.EnsureSuccessStatusCode();
            string responseBody = await response.Content.ReadAsStringAsync();

            // 출력
            Console.WriteLine(responseBody);
        }
        catch(HttpRequestException e)
        {
            Console.WriteLine("\n예외 발생");
            Console.WriteLine("Message :"+ e.Message);
        }
    }
}
```
올바로 실행되면, 콘솔에 해당 웹페이지의 HTML 소스가 출력됩니다.

## Deep Dive

웹 페이지 다운로드는 웹 크롤링(Web Crawling)의 기본적인 요소입니다. 웹 크롤링이란 인터넷의 웹 페이지를 방문하며, 그 상에서 존재하는 내용을 사본으로 만드는 기술을 의미합니다.

현대적인 .NET 환경에서는 `HttpClient` 클래스 뿐만 아니라, `WebClient` 클래스를 이용하여 웹페이지를 다운로드받을 수도 있습니다. 간편하게 사용할 수 있지만, `HttpClient` 클래스가 더 다양한 기능을 제공하며, 더 효율적인 성능을 보입니다.

이 구현에서는 `async` 와 `await` 키워드를 사용하여 비동기 작업을 수행합니다. 이를 통해 웹페이지 다운로드 동안 프로그램이 멈추는 것을 방지하며, 다른 작업을 동시에 처리할 수 있습니다.

## 참고 자료

- Microsoft 공식 문서 [HttpClient 클래스](https://docs.microsoft.com/ko-kr/dotnet/api/system.net.http.httpclient)
- [.NET Core 가이드] (https://docs.microsoft.com/ko-kr/dotnet/csharp/) 
- [WebClient 클래스](https://docs.microsoft.com/ko-kr/dotnet/api/system.net.webclient)
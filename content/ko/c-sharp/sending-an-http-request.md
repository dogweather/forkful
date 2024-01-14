---
title:                "C#: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜
HTTP 요청을 보내는 것이 왜 필요한지 궁금하신가요? HTTP 요청은 웹 서버에게 정보를 요청하고 응답을 받기위해 사용되는 효율적인 방법입니다. 예를 들어, 웹 브라우저에서 웹 페이지를 열거나, API를 호출하여 데이터를 가져오는 등 여러 가지 용도로 사용될 수 있습니다.

## 방법
아래 코드 블록에서는 간단한 C# 예제를 통해 HTTP 요청을 보내는 방법을 보여드리겠습니다. 이 예제는 .NET core 버전을 기준으로 작성되었습니다.

```C#
// 필요한 네임스페이스를 추가합니다.
using System;
using System.Net;
using System.IO;

public class Program
{
    public static void Main()
    {
        // 요청할 URL을 설정합니다.
        string url = "https://jsonplaceholder.typicode.com/posts/1";

        // HttpWebRequest 객체를 생성합니다.
        HttpWebRequest request = (HttpWebRequest)WebRequest.Create(url);

        // Get 요청 방식을 설정합니다.
        request.Method = "GET";

        // 응답을 받기 위해 HttpWebResponse 객체를 생성합니다.
        HttpWebResponse response = (HttpWebResponse)request.GetResponse();

        // 응답 데이터를 읽기 위해 Stream 객체를 생성합니다.
        Stream dataStream = response.GetResponseStream();

        // 응답 데이터를 읽을 StreamReader 객체를 생성합니다.
        StreamReader reader = new StreamReader(dataStream);

        // 응답을 문자열 형태로 받아옵니다.
        string responseFromServer = reader.ReadToEnd();

        // 받아온 데이터를 콘솔에 출력합니다.
        Console.WriteLine(responseFromServer);

        // 생성한 객체들을 모두 닫아줍니다.
        reader.Close();
        dataStream.Close();
        response.Close();
    }
}
```

위의 코드를 실행하면 다음과 같은 결과를 얻을 수 있습니다.

```
{
  userId: 1,
  id: 1,
  title: "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
  body: "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit ...",
}
```

## 딥 다이브
HTTP 요청은 여러 가지 다양한 설정을 통해 더욱 유연하게 사용할 수 있습니다. 예를 들어, POST 요청을 보내거나 특정 데이터를 요청하는 등의 기능을 추가할 수 있습니다. 또한 보안적인 측면에서도 HTTPS 프로토콜을 사용하여 더 안전하게 요청을 보낼 수 있습니다. 따라서 필요한 기능에 맞게 HTTP 요청을 보내는 방법을 익히는 것은 매우 중요합니다.

## 참고 자료
- [HTTP 요청 보내기](https://docs.microsoft.com/ko-kr/dotnet/api/system.net.httpwebrequest?view=netcore-3.1)
- [ASP.NET Core에서 HTTP 요청 처리하기](https://docs.microsoft.com/ko-kr/aspnet/core/fundamentals/http-requests?view=aspnetcore-3.1)
- [HTTP 신호와 메시지 형식](https://hahohi.tistory.com/13)
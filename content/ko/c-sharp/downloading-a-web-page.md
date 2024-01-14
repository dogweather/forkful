---
title:                "C#: 웹페이지 다운로드하기"
simple_title:         "웹페이지 다운로드하기"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 왜
웹 페이지를 다운로드 받는 이유는 다양합니다. 가장 일반적인 이유는 웹 스크래핑이나 데이터 마이닝과 같은 작업을 할 때 필요하기 때문입니다. 또한 인터넷 연결이 없는 환경에서 웹 페이지를 오프라인으로 볼 수도 있습니다. 다른 이유로는 웹 페이지의 소스 코드를 분석하거나 특정 이미지나 파일을 다운로드하는 경우 등이 있습니다.

## 사이트 다운로드 하는 방법
```C#
using System;
using System.Net;

string url = "https://www.example.com"; // 다운로드할 웹 사이트의 URL

using (WebClient client = new WebClient())
{
    client.DownloadFile(url, "myfile.html"); // 웹 페이지를 myfile.html로 다운로드
    Console.WriteLine("다운로드가 완료되었습니다.");
}
```
위의 코드는 C#을 사용하여 웹 페이지를 다운로드하는 간단한 예제입니다. 우선, 사용할 WebClient 클래스를 선언합니다. 그리고 다운로드할 웹 사이트의 URL을 지정합니다. 마지막으로 DownloadFile 메서드를 호출하여 웹 페이지를 다운로드하고, 콘솔 창에 다운로드 완료 메시지를 출력합니다.

```C#
using System;
using System.Net;

string url = "https://www.example.com"; // 다운로드할 웹 사이트의 URL

using (WebClient client = new WebClient())
{
    string html = client.DownloadString(url); // 웹 페이지의 내용을 string 변수에 저장
    Console.WriteLine(html); // 웹 페이지의 내용을 콘솔 창에 출력
}
```
만약 다운로드한 웹 페이지의 내용을 string 변수에 저장하여 원하는 작업을 하고 싶다면, DownloadString 메서드를 사용하면 됩니다. 위의 코드는 웹 페이지의 내용을 콘솔 창에 출력하는 예제입니다.

## 깊게 파보기
웹 페이지를 다운로드하는 것은 간단해 보이지만, 실제로는 여러 가지 작업이 필요합니다. 먼저, 웹 사이트의 URL을 알아야 하고, 해당 사이트에 접근할 수 있는 인증 정보가 있어야 합니다. 또한 서버에서 오는 응답 코드를 확인하고, 다운로드할 파일의 경로를 지정해야 합니다. 예를 들어, 웹 사이트의 HTML 코드만을 다운로드하는 것도 가능하지만, CSS, JavaScript 등의 파일도 함께 다운로드하여 웹 페이지를 완벽하게 표현할 수 있도록 해야 합니다.

## 참고 자료
- [WebClient Class (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.net.webclient?view=net-5.0)
- [How to download a file from a website in C# (Code Maze)](https://code-maze.com/download-file-from-website/)
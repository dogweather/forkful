---
title:                "주소가 기본 인증으로 HTTP 요청 보내기"
html_title:           "C#: 주소가 기본 인증으로 HTTP 요청 보내기"
simple_title:         "주소가 기본 인증으로 HTTP 요청 보내기"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜

*왜* 누군가가 기본 인증을 사용하여 HTTP 요청을 보낼까요?

기본 인증은 웹 서비스에서 사용자의 인증 정보를 안전하게 전달하기 위한 방법입니다. 이는 입력한 사용자 이름과 비밀번호를 암호화된 형태로 서버에 전송하여 인증된 사용자만이 서비스를 이용할 수 있도록 합니다. 따라서 기본 인증은 보안적 측면에서 중요하며, 많은 웹 애플리케이션에서 사용됩니다.

## 어떻게

```
C# 코드 예시
using System;
using System.Net;
class BasicAuthExample
{
    static void Main()
    {
        Console.WriteLine("비밀번호 보호 페이지에 접속");
        HttpWebRequest request = (HttpWebRequest)WebRequest.Create("https://www.example.com/protected_page");
        
        Console.Write("사용자 이름: ");
        string username = Console.ReadLine();
        Console.Write("비밀번호: ");
        string password = Console.ReadLine();
        
        // 기본 인증 헤더 생성
        string authString = $"{username}:{password}";
        string encodedAuthString = Convert.ToBase64String(System.Text.ASCIIEncoding.ASCII.GetBytes(authString));
        string authHeader = $"Basic {encodedAuthString}";
        
        // 헤더에 추가
        request.Headers.Add("Authorization", authHeader);
        
        // 응답 받기
        HttpWebResponse response = (HttpWebResponse)request.GetResponse();
        Console.WriteLine($"응답 코드: {response.StatusDescription}");
        
        Stream dataStream = response.GetResponseStream();
        StreamReader reader = new StreamReader(dataStream);
        string responseFromServer = reader.ReadToEnd();
        
        // 응답 출력
        Console.WriteLine("\n응답:");
        Console.WriteLine(responseFromServer);
        
        // 닫기
        reader.Close();
        dataStream.Close();
        response.Close();
    }
}
```

```
출력 예시
비밀번호 보호 페이지에 접속
사용자 이름: John
비밀번호: 1234
응답 코드: OK

응답:
<Protected Page Content>
```

## 딥 다이브

기본 인증은 웹 서비스에서 인증 단계에 가장 보편적으로 사용되는 방법 중 하나입니다. 이는 간단하고 쉽게 구현할 수 있어서 더 많은 웹 서비스에서 사용되고 있으며, 보안 측면에서도 충분한 수준의 보호를 제공합니다. 하지만 보안을 강화하고 싶다면 다른 인증 방법을 추가로 사용하는 것이 좋습니다.

## 참고

[MSDN - HttpWebRequest 클래스](https://docs.microsoft.com/ko-kr/dotnet/api/system.net.httpwebrequest?view=netframework-4.8)

[MSDN - HttpWebResponse 클래스](https://docs.microsoft.com/ko-kr/dotnet/api/system.net.httpwebresponse?view=netframework-4.8)

[C# 기본 인증 사용하기](https://www.codementor.io/@nmaruti/csharp-use-http-basic-authentication-for-securely-consume-restful-api-7zf2llgz9)

[C#으로 HTTP 요청을 보내는 방법](https://www.fluentbytes.com/c-how-to-send-a-http-put-keepalive-headers-to-owncloud/)
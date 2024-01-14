---
title:                "C#: 기본 인증을 이용하여 http 요청 보내기"
simple_title:         "기본 인증을 이용하여 http 요청 보내기"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜 HTTP 요청에 기본 인증을 사용해야 할까요?
HTTP 요청에 기본 인증을 사용하는 이유는 보안과 인증입니다. 그리고 당신의 애플리케이션은 다른 서비스와 상호작용하기 위해서도 기본 인증을 사용할 수 있습니다.

## 어떻게 하면 HTTP 요청에 기본 인증을 보내나요?

```C#
var request = (HttpWebRequest)WebRequest.Create("https://example.com/api");
request.Headers["Authorization"] = "Basic " + Convert.ToBase64String(Encoding.ASCII.GetBytes("username:password"));
var response = (HttpWebResponse)request.GetResponse();
Console.WriteLine("Status: " + response.StatusDescription);
```

위의 예제 코드는 `HttpWebRequest` 클래스를 사용하여 HTTP 요청을 만들고, 기본 인증 헤더를 추가해주고, 요청한 주소로부터 응답을 받아와서 상태코드를 확인하는 간단한 예제입니다. 요청하는 주소와 인증에 필요한 정보를 바꾸면 다른 서비스에도 적용할 수 있습니다.

## 기본 인증으로 HTTP 요청 보내기의 심화 학습

기본 인증은 사용자명과 비밀번호를 암호화하여 인증 헤더에 담는 방식으로 보안을 제공합니다. 이를 위해 `Authorization` 헤더에 `Basic`이라는 타입을 지정하고, 사용자명과 비밀번호를 `username:password` 형태로 Base64로 인코딩하여 전송합니다. 이러한 보안 메커니즘은 HTTPS와 함께 사용할 경우 더욱 강력해집니다.

## 더 알아보기

- [Microsoft Docs C# HttpWebRequest 클래스 페이지](https://docs.microsoft.com/ko-kr/dotnet/api/system.net.httpwebrequest)
- [MDN HTTP 기본 인증 문서 (영문)](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
- [C# Base64 인코딩 관련 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.convert.tobase64string)
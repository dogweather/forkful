---
title:                "기본 인증을 사용하여 http 요청 보내기"
html_title:           "C#: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

HTTP 요청을 기본 인증과 함께 보내는 것은 서버에 데이터를 요청할 때 사용되는 방법입니다. 이를 통해 프로그래머들은 사용자를 인증하고, 데이터를 안전하게 전송할 수 있습니다.

## 어떻게:

```C#
// 예제 코드
var request = (HttpWebRequest)WebRequest.Create("http://www.example.com");
request.Method = "GET";
request.Headers["Authorization"] = "Basic " + Convert.ToBase64String(Encoding.Default.GetBytes(username + ":" + password));
var response = (HttpWebResponse)request.GetResponse();
Console.WriteLine(response.StatusCode);
```
```
// 예상 출력 결과
200 OK
```

## 깊은 곳을 파헤쳐보기:

HTTP 기본 인증은 인터넷 정보 교류의 초기 단계 중 하나로 개발되었으며, 인증 헤더에 사용자 정보를 실어 보내는 방식입니다. 이 외에도 다른 인증 방식으로는 OAuth, JWT 등이 있습니다. HTTP 기본 인증은 간단하지만 보안에 취약한 단점이 있으므로, 안전한 데이터 전송을 위해서는 다른 방식을 고려해야 합니다. 이를 구현하기 위해서는 WebRequest 클래스를 사용하면 됩니다.

## 다른 정보 보기:

- [HTTP 기본 인증](https://www.w3.org/Protocols/HTTP/Basic.html) : 공식 문서에서 더 자세한 정보를 확인할 수 있습니다.
- [C#에서 HTTP 요청 보내기](https://docs.microsoft.com/ko-kr/dotnet/api/system.net.httpwebrequest?view=net-5.0) : MSDN 문서에서 WebRequest 클래스와 기본 인증을 포함한 다양한 HTTP 요청 방법을 알아볼 수 있습니다.
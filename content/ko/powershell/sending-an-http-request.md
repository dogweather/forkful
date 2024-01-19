---
title:                "HTTP 요청 보내기"
html_title:           "Clojure: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

HTTP 요청 전송이란 클라이언트가 웹 서버로 정보를 요청하는 방식입니다. 프로그래머가 이를 수행하는 이유는 웹 서비스와 정보를 주고 받기 위해서입니다.

## 어떻게 하는가:

```PowerShell
# Invoke-WebRequest cmdlet를 사용해 HTTP GET 요청을 보냅니다.
$response = Invoke-WebRequest -Uri "http://example.com"

# 응답 본문을 출력합니다.
$response.Content
```

이 코드는 `http://example.com` 사이트로 HTTP GET 요청을 보내고 응답을 출력하는 예제입니다.

## 깊이 있게 살펴보기:

HTTP 요청은 1990년까지 거슬러 올라갈 수 있는 웹의 기본 요소입니다. PowerShell에서 `Invoke-WebRequest` 이외에도 `System.Net.WebRequest` 클래스를 통해 HTTP 요청을 보낼 수 있습니다. 그러나 `Invoke-WebRequest`는 사용하기 간단하고 응답이 PowerShell 객체로 쉽게 접근 가능하기 때문에 더 인기가 많습니다.

## 참고자료:

- 공식 PowerShell 문서: [Invoke-WebRequest](https://docs.microsoft.com/ko-kr/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1)
- HTTP 요청의 이해: [HTTP request methods (MDN)](https://developer.mozilla.org/ko/docs/Web/HTTP/Methods)
- .NET API 문서: [System.Net.WebRequest](https://docs.microsoft.com/ko-kr/dotnet/api/system.net.webrequest?view=net-5.0)
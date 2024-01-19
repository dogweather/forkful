---
title:                "기본 인증을 이용한 HTTP 요청 보내기"
html_title:           "Arduino: 기본 인증을 이용한 HTTP 요청 보내기"
simple_title:         "기본 인증을 이용한 HTTP 요청 보내기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 할까요?

HTTP 요청에 기본 인증을 보내는 것은 웹 서버에 안전하게 데이터를 전송하는 방법입니다. 프로그래머들이 이것을 사용하는 이유는 원격 서버를 통해 데이터를 안전하게 교환하거나 API를 호출하기 위해서입니다.

## 사용 방법:

먼저 인증 정보를 인코딩해야 합니다. 이는 웹 서버가 해당 요청을 검증할 수 있게 해줍니다.

```PowerShell
$user = "Username"
$pass = "Password"
$pair = "$($user):$($pass)"
$encoded = [System.Convert]::ToBase64String([System.Text.Encoding]::ASCII.GetBytes($pair))
```

이제 생성된 기본 인증 문자열을 HTTP 헤더에 추가하여 요청을 보낼 수 있습니다.

```PowerShell
$uri = "http://example.com"
$headers = @{ Authorization = "Basic $encoded" }
$response = Invoke-RestMethod -Uri $uri -Headers $headers
$response
```

## 깊이 파보기

기본 인증은 HTTP/1.0에서 처음 소개되었으며, 웹 서버와 클라이언트 간의 통신을 보호하기 위한 인증 메커니즘입니다. 그러나 정보가 Base64 인코딩만 사용되기 때문에, 잠재적인 보안 문제가 있습니다.

대안으로서, 계정 정보를 encrypt하는 더 안전한 방법들이 사용될 수 있습니다. 가장 일반적인 방법은 OAuth이며, 토큰 기반의 인증 시스템입니다.

구현에 관한 자세히 말하자면, 기본 인증 헤더는 'Authorization'라는 헤더 필드와 'Basic'이라는 키워드, 그리고 콜론 `:`으로 분리된 username과 password의 Base64 인코딩을 포함합니다.

## 참고자료

- [Microsoft Docs: Invoke-RestMethod](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod?view=powershell-7.1)
- [RFC 7617: Basic Authentication Scheme](https://tools.ietf.org/html/rfc7617)
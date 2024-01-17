---
title:                "기본 인증으로 http 요청 보내기"
html_title:           "PowerShell: 기본 인증으로 http 요청 보내기"
simple_title:         "기본 인증으로 http 요청 보내기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

[서문]

안녕하세요! 오늘은 PowerShell을 사용하여 HTTP 요청을 보낼 때 기본 인증을 사용하는 방법에 대해 알아보겠습니다. HTTP 요청이란 서버에 정보를 요청하거나 전송하기 위해 사용되는 프로토콜이며, 기본 인증은 사용자의 신원을 확인하기 위해 사용되는 인증 방식입니다. 그렇다면 프로그래머들은 왜 기본 인증을 사용할까요? 이에 대해서는 아래에서 자세히 설명하겠습니다.

## 무엇 & 왜?

HTTP 요청을 보낼 때 기본 인증이란 사용자의 신원을 확인하기 위해 사용되는 인증 방식입니다. 이 방식은 보안 수준이 낮기 때문에 민감한 정보를 주고받는 경우에는 적절하지 않습니다. 하지만 간단한 인증만 필요한 경우에는 편리하게 사용할 수 있습니다. 프로그래머들은 자신이 만든 프로그램에 기본 인증을 추가하여 사용자의 신원을 확인하고, 필요한 정보를 안전하게 요청하거나 전송할 수 있습니다.

## 방법:

```PowerShell
# 인증 정보를 변수에 저장합니다.
$credential = [System.Convert]::ToBase64String([System.Text.Encoding]::ASCII.GetBytes("username:password"))

# HTTP 요청을 보냅니다.
Invoke-RestMethod -Uri https://example.com/api -Method Get -Headers @{Authorization = "Basic $credential"}
```

위의 예제에서는 `$credential` 변수에 `username:password` 형식으로 인증 정보를 저장하고, `Invoke-RestMethod`를 사용하여 HTTPS 요청을 보냅니다. 이때 `-Headers` 옵션을 사용하여 요청 헤더에 `Authorization` 값을 추가하여 기본 인증을 수행합니다.

## 깊게 파보기:

기본 인증은 HTTP 프로토콜의 스팩 RFC 2617에서 정의되었으며, 앞서 언급한 것처럼 보안 수준이 낮기 때문에 개인 정보와 같은 민감한 정보를 주고받는 경우에는 사용하지 않는 것이 좋습니다. 대안으로는 보다 강력한 인증 방법인 OAuth와 같은 프로토콜을 사용할 수 있습니다. 또한, `-Headers` 옵션 대신 `-Credential` 옵션을 사용하여 기본 인증을 수행할 수도 있습니다. 이와 관련된 자세한 내용은 아래의 링크를 참고하시기 바랍니다.

## 참고:

- [RFC 2617 - HTTP Authentication: Basic and Digest Access Authentication](https://tools.ietf.org/html/rfc2617)
- [OAuth](https://oauth.net/)
- [Invoke-RestMethod](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod?view=powershell-7.1)

이상으로 PowerShell을 사용하여 HTTP 요청을 보낼 때 기본 인증을 사용하는 방법에 대해 알아보았습니다. 프로그래머들은 이를 활용하여 보다 편리하게 서버와 통신할 수 있습니다. 감사합니다!
---
date: 2024-01-20 18:02:49.112566-07:00
description: "HTTP \uC694\uCCAD\uC740 \uC6F9 \uC11C\uBC84\uC640 \uD1B5\uC2E0\uC758\
  \ \uAE30\uBCF8\uC785\uB2C8\uB2E4. \uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD558\
  \uC5EC \uBCF4\uC548 \uC815\uBCF4\uB97C \uD3EC\uD568\uD55C \uC694\uCCAD\uC744 \uBCF4\
  \uB0BC \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uC774\
  \ \uBC29\uC2DD\uC744 \uC885\uC885 \uC0AC\uC6A9\uD574 \uB370\uC774\uD130\uB97C \uC548\
  \uC804\uD558\uAC8C \uC804\uC1A1\uD558\uACE0 API \uC11C\uBE44\uC2A4\uC5D0 \uC811\uADFC\
  \uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-02-25T18:49:52.536889-07:00'
model: gpt-4-1106-preview
summary: "HTTP \uC694\uCCAD\uC740 \uC6F9 \uC11C\uBC84\uC640 \uD1B5\uC2E0\uC758 \uAE30\
  \uBCF8\uC785\uB2C8\uB2E4. \uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD558\uC5EC\
  \ \uBCF4\uC548 \uC815\uBCF4\uB97C \uD3EC\uD568\uD55C \uC694\uCCAD\uC744 \uBCF4\uB0BC\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uC774 \uBC29\
  \uC2DD\uC744 \uC885\uC885 \uC0AC\uC6A9\uD574 \uB370\uC774\uD130\uB97C \uC548\uC804\
  \uD558\uAC8C \uC804\uC1A1\uD558\uACE0 API \uC11C\uBE44\uC2A4\uC5D0 \uC811\uADFC\uD569\
  \uB2C8\uB2E4."
title: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD55C HTTP \uC694\uCCAD \uBCF4\
  \uB0B4\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
HTTP 요청은 웹 서버와 통신의 기본입니다. 기본 인증을 사용하여 보안 정보를 포함한 요청을 보낼 수 있습니다. 프로그래머는 이 방식을 종종 사용해 데이터를 안전하게 전송하고 API 서비스에 접근합니다.

## How to: (방법)
```PowerShell
# 사용자 이름과 비밀번호를 정의합니다.
$user = 'your_username'
$pass = 'your_password'

# Base64로 인코드합니다.
$base64AuthInfo = [Convert]::ToBase64String([Text.Encoding]::ASCII.GetBytes(("{0}:{1}" -f $user,$pass)))

# HTTP 요청을 설정합니다.
$headers = @{
    Authorization=("Basic {0}" -f $base64AuthInfo)
}

# 요청을 보내고 결과를 받습니다.
$response = Invoke-RestMethod -Uri 'http://example.com/api/data' -Method Get -Headers $headers

# 응답을 출력합니다.
$response
```
실행 결과는 사이트나 데이터에 따라 달라집니다. 잘된다면, 요청한 데이터가 표시됩니다.

## Deep Dive (심층 분석)
역사적으로, 기본 인증(Basic Auth)은 가장 오래된 HTTP 인증 방식 중 하나입니다. 사용이 간편하지만 최근에는 보안 문제로 인해 일부 더 안전한 대안들(예: OAuth)이 더 많이 추천됩니다. 기변 인증은 사용자 이름과 비밀번호를 Base64로 인코딩하여 `Authorization` 헤더 안에 넣어 서버에 전송합니다. 웹 서버는 이 정보를 해석해서 사용자를 인증합니다.

단, HTTP가 아닌 HTTPS 프로토콜 위에서 Basic Auth를 사용할 때만 안전합니다. 이 방식은 비밀번호를 암호화하지 않고 단순히 인코딩만 합니다. 따라서 암호화되지 않은 HTTP 통신에서 사용할 시 정보가 노출될 위험이 있습니다.

## See Also (참고 자료)
- [Invoke-RestMethod documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod?view=powershell-7.1)
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [OAuth 2.0](https://oauth.net/2/)
- [Secure your web API using Basic authentication](https://docs.microsoft.com/en-us/aspnet/web-api/overview/security/basic-authentication)

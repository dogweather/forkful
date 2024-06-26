---
date: 2024-01-20 18:02:49.112566-07:00
description: "How to: (\uBC29\uBC95) \uC2E4\uD589 \uACB0\uACFC\uB294 \uC0AC\uC774\uD2B8\
  \uB098 \uB370\uC774\uD130\uC5D0 \uB530\uB77C \uB2EC\uB77C\uC9D1\uB2C8\uB2E4. \uC798\
  \uB41C\uB2E4\uBA74, \uC694\uCCAD\uD55C \uB370\uC774\uD130\uAC00 \uD45C\uC2DC\uB429\
  \uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.202249-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) \uC2E4\uD589 \uACB0\uACFC\uB294 \uC0AC\uC774\uD2B8\uB098\
  \ \uB370\uC774\uD130\uC5D0 \uB530\uB77C \uB2EC\uB77C\uC9D1\uB2C8\uB2E4."
title: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD55C HTTP \uC694\uCCAD \uBCF4\
  \uB0B4\uAE30"
weight: 45
---

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

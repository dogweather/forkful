---
date: 2024-01-20 18:00:15.680099-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) PowerShell\uC5D0\uC11C\
  \ HTTP \uC694\uCCAD\uC744 \uBCF4\uB0B4\uB294 \uAC00\uC7A5 \uAC04\uB2E8\uD55C \uBC29\
  \uBC95\uC740 `Invoke-WebRequest` \uBA85\uB839\uC5B4\uB97C \uC0AC\uC6A9\uD558\uB294\
  \ \uAC70\uC5D0\uC694. \uC608\uC2DC\uB97C \uB4E4\uC5B4 \uBCFC\uAE4C\uC694?."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.199178-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) PowerShell\uC5D0\uC11C HTTP \uC694\
  \uCCAD\uC744 \uBCF4\uB0B4\uB294 \uAC00\uC7A5 \uAC04\uB2E8\uD55C \uBC29\uBC95\uC740\
  \ `Invoke-WebRequest` \uBA85\uB839\uC5B4\uB97C \uC0AC\uC6A9\uD558\uB294 \uAC70\uC5D0\
  \uC694."
title: "HTTP \uC694\uCCAD \uBCF4\uB0B4\uAE30"
weight: 44
---

## How to: (어떻게 하나요?)
PowerShell에서 HTTP 요청을 보내는 가장 간단한 방법은 `Invoke-WebRequest` 명령어를 사용하는 거에요. 예시를 들어 볼까요?

```PowerShell
# GET 요청
$response = Invoke-WebRequest -Uri "http://example.com/api/data"
$response.Content

# POST 요청
$body = @{ key1 = 'value1'; key2 = 'value2' } | ConvertTo-Json
$response = Invoke-WebRequest -Uri "http://example.com/api/post" -Method POST -Body $body -ContentType "application/json"
$response.Content
```

실행 예시:
```
[
  {
    "id": 1,
    "name": "홍길동",
    "email": "hong@example.com"
  }
]
```

## Deep Dive (심층 탐구)
PowerShell 2.0 버전부터 `Invoke-WebRequest`와 `Invoke-RestMethod` 같은 명령어가 도입되었어요. `Invoke-RestMethod`는 JSON, XML 같은 데이터를 자동으로 파싱해줘서 RESTful API와 작업할 때 더 편리해요. 대안으로는 .NET의 `System.Net.Http.HttpClient`를 사용할 수 있고, 이를 통해 더 복잡한 HTTP 요청을 할 수 있어요.

예를 들어, 고급 HTTP 헤더를 설정하거나 인증 작업을 필요로 할 때 `HttpClient`를 사용하면 좋습니다. PowerShell 6.0 이후에 나온 버전에서는 `Invoke-RestMethod`와 `Invoke-WebRequest` 모두 크로스 플랫폼 지원을 갖췄죠. 이제는 PowerShell Core를 사용하여 리눅스나 macOS에서도 동일한 명령어를 실행할 수 있습니다.

## See Also (더 보기)
- [PowerShell `Invoke-WebRequest` 공식 문서](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [PowerShell `Invoke-RestMethod` 공식 문서](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [.NET `HttpClient` 클래스 문서](https://docs.microsoft.com/dotnet/api/system.net.http.httpclient)

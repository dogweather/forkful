---
title:                "HTTP 요청 보내기"
date:                  2024-01-20T18:00:15.680099-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP 요청 보내기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
HTTP 요청을 보낸다는 것은 웹 서버에 정보를 요구하거나 제출하는 행위입니다. 프로그래머들이 이를 사용하는 이유는 데이터를 받거나, 웹 서비스와 상호작용하거나, API를 통해 기능을 실행하기 위해서죠.

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
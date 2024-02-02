---
title:                "HTTP 요청 보내기"
date:                  2024-02-01T22:02:19.638091-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTTP 요청 보내기"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/vba/sending-an-http-request.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

Visual Basic for Applications(VBA)에서 HTTP 요청을 보내는 것은 HTTP를 통해 웹 자원이나 웹 서비스에 프로그래밍 방식으로 접근하여 데이터를 가져오거나 온라인 API와 상호 작용하거나, Excel, Access 또는 맞춤형 VBA 솔루션과 같은 VBA 활성화 애플리케이션 내에서 프로그래밍 방식으로 양식을 제출하기 위해 수행하는 프로세스입니다.

## 방법:

VBA에서 HTTP 요청을 보내는 열쇠는 `Microsoft XML, v6.0` 라이브러리(또는 시스템에 따라 이전 버전)를 사용하는 것입니다. 프로젝트에서 이 참조가 활성화되어 있는지 확인하려면 VBA 편집기에서 도구 > 참조로 이동하여 `Microsoft XML, v6.0`을 확인하십시오.

간단한 HTTP GET 요청을 보내는 방법은 다음과 같습니다:

```vb
Dim httpRequest As Object
Set httpRequest = CreateObject("MSXML2.XMLHTTP.6.0")

With httpRequest
    .Open "GET", "https://api.example.com/data", False
    .send
    If .Status = 200 Then
        Debug.Print .responseText
    Else
        Debug.Print "Error: " & .Status & " - " & .statusText
    End If
End With
```

서버에 데이터(예: JSON)를 보내야 하는 POST 요청의 경우:

```vb
Dim httpRequest As Object, postData As String
Set httpRequest = CreateObject("MSXML2.XMLHTTP.6.0")
postData = "{""key"":""value""}"

With httpRequest
    .Open "POST", "https://api.example.com/submit", False
    .setRequestHeader "Content-Type", "application/json"
    .send postData
    If .Status = 200 Then
        Debug.Print .responseText
    Else
        Debug.Print "Error: " & .Status & " - " & .statusText
    End If
End With
```

성공적인 요청에 대한 샘플 출력은 JSON 문자열이나 상호 작용하는 API 또는 웹페이지에 따라 HTML 페이지일 수 있습니다:

```
{"data": "서버로부터의 응답입니다"}
```

## 심층 분석

여기에서 보여준 방법은 Microsoft XML Core Services(MSXML)의 일부인 `MSXML2.XMLHTTP` 객체를 활용합니다. 이는 VBA 개발자들이 XML 기반 연산을 수행할 수 있도록 하기 위해 도입되었고, 시간이 지남에 따라 XML 데이터와 직접 작업하지 않을 때조차 HTTP 요청에 대한 일반적인 도구가 되었습니다. 나이에도 불구하고 VBA에서 간단한 웹 상호 작용을 위한 신뢰할 수 있는 옵션으로 남아 있습니다.

그러나, VBA와 그 HTTP 요청 메커니즘은 현대 프로그래밍 환경에서 발견되는 강력함과 유연성이 부족합니다. 예를 들어, 비동기 요청을 처리하거나 고급 HTTP 기능(예: 웹소켓이나 서버에서 보내는 이벤트)이 필요한 애플리케이션 내에서 작업하는 것은 VBA의 범위를 벗어납니다. 더 복잡한 웹 통합 프로젝트를 진행할 때, 개발자들은 종종 외부 라이브러리나 도구를 활용하거나 웹 스크래핑 기술을 통해 브라우저 행동을 자동화하지만, 이들은 해결책보다는 우회적인 방법입니다.

Python의 `requests` 라이브러리나 Node.js에서 실행되는 JavaScript와 같은 언어 및 환경은 비동기 작업, JSON 처리의 용이성, 다양한 웹 기술에 대한 광범위한 지원을 포함하여 상자 밖에서 더 강력하고 다재다능한 HTTP 요청 기능을 제공합니다. Microsoft 생태계에 깊이 뿌리내린 개발자들은 더 복잡한 웹 상호 작용을 요구하는 작업을 위해 PowerShell 또는 C#으로 전환을 고려할 수 있으며, .NET의 광범위한 네트워크 프로그래밍 기능을 활용할 수 있습니다.

따라서, VBA의 HTTP 요청 기능은 간단한 쿼리 및 데이터 가져오기 작업에 적합하나, 프로젝트의 요구 사항이 복잡하고 현대적인 웹 환경으로 발전함에 따라 대안을 탐색하는 것이 중요해집니다.

---
title:                "웹 페이지 다운로드하기"
date:                  2024-01-20T17:44:52.732974-07:00
model:                 gpt-4-1106-preview
simple_title:         "웹 페이지 다운로드하기"

category:             "PowerShell"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
웹 페이지를 다운로드한다는 것은 인터넷 상의 정보를 로컬 컴퓨터로 가져오는 작업입니다. 프로그래머는 데이터 분석, 모니터링, 백업 또는 자동화된 테스트를 위해 이를 수행합니다.

## How to: (어떻게 하나요?)
```PowerShell
# Invoke-WebRequest를 사용하여 웹 페이지 내용을 다운로드합니다.
$response = Invoke-WebRequest -Uri "http://example.com"

# 웹 페이지의 HTML 내용을 파일로 저장합니다.
$response.Content > "example.html"

# 결과 확인
Get-Content -Path "example.html"
```
실행하면 'example.com'의 HTML 내용이 'example.html' 파일로 저장됩니다.

## Deep Dive (심층적인 분석)
과거에는 웹 페이지를 다운로드하기 위하여 `wget`이나 `curl`과 같은 독립적인 도구들이 많이 사용되었습니다. PowerShell이 등장하면서, `Invoke-WebRequest`나 `Invoke-RestMethod` 같은 명령어를 통해 직접 HTTP 요청을 보내고 응답을 받을 수 있게 되었습니다. 이들은 RESTful API 호출에도 사용되기 때문에 웹 자동화와 API 통합 두 분야에서 모두 유용합니다. 대안으로는 .NET Framework의 클래스를 사용할 수 있지만, PowerShell 명령어는 사용이 더 간단합니다.

## See Also (참고 자료)
- [Invoke-WebRequest Documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [PowerShell Gallery](https://www.powershellgallery.com/)

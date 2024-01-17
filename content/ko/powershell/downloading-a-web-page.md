---
title:                "웹 페이지 다운로드"
html_title:           "PowerShell: 웹 페이지 다운로드"
simple_title:         "웹 페이지 다운로드"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
파워쉘을 사용하여 웹 페이지를 다운로드하는 것은 매우 쉬운 일입니다. 프로그래머들은 다양한 이유로 인해 이 작업을 수행합니다. 예를 들어, 웹 사이트에서 데이터를 스크랩하여 정보를 수집하거나, 오픈 소스 프로젝트를 다운로드하여 코드를 분석하는 등의 목적으로 이 작업을 수행합니다.

## 방법:
다음은 파워쉘을 사용하여 웹 페이지를 다운로드하는 방법의 예시입니다.
```
# 웹 페이지의 URL 저장
$url = "https://www.example.com/"

# 다운로드할 파일 경로 지정 후 해당 폴더에 저장
$path = "C:\Users\Username\Documents\example.html"

# Invoke-WebRequest 명령을 사용하여 웹 페이지 다운로드
Invoke-WebRequest -Uri $url -OutFile $path
```

위의 코드를 실행하면 예시 웹 페이지가 C드라이브의 해당 경로에 example.html 파일로 다운로드됩니다.

## 심층 분석:
- 웹 페이지 다운로드는 프로그래밍의 기본 기술 중 하나입니다.
- 파워쉘의 Invoke-WebRequest 명령은 HTTP 요청을 생성하여 웹 페이지를 다운로드합니다.
- 웹 페이지 다운로드를 사용하여 다양한 목적을 달성할 수 있습니다.
- 다른 언어와 라이브러리를 사용하여 웹 페이지를 다운로드하는 다양한 방법이 있습니다.

## 관련 자료:
- [Microsoft Docs](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7) 에서 파워쉘의 Invoke-WebRequest 명령에 대한 자세한 정보를 확인할 수 있습니다.
- [Codecademy](https://www.codecademy.com/learn/learn-powershell) 에서 파워쉘을 배워 간단한 프로그램부터 시작해 복잡한 스크립트를 작성할 수 있습니다.
- [YouTube](https://www.youtube.com/watch?v=WRMGIQSuFQY)에서 파워쉘을 사용하여 웹 페이지를 다운로드하는 방법에 대한 동영상 튜토리얼을 시청할 수 있습니다.
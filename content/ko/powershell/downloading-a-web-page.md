---
title:                "웹 페이지 다운로드하기"
html_title:           "Bash: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

웹 페이지를 다운로드하는 것은, 웹 서버에서 HTML 문서를 내 컴퓨터에 저장하는 것을 말합니다. 프로그래머들이 이를 수행하는 이유는 여러 가지지만 대표적으로 웹 사이트의 정보를 분석하거나, 자동화된 매크로 작업을 할 필요가 있을 때입니다.

## 어떻게 하는가:

웹 페이지를 다운로드하는 PowerShell 코드는 아래와 같습니다.   

```PowerShell
$webClient = New-Object System.Net.WebClient
$webClient.DownloadFile("http://example.com", "C:\temp\example.html")
```

위의 코드를 실행하면, "http://example.com" 웹페이지가 "C:\temp\example.html"라는 위치에 다운로드 됩니다.

## Deep Dive:

PowerShell에서 웹 페이지를 다운로드하는 방법은 다른 프로그래밍 언어와 비슷하다는 것을 알 수 있습니다. 

(1) 웹 페이지를 다운로드하는 기능은 웹 크롤링 또는 스크레이핑 기법의 핵심적인 부분으로, 처음에는 전 세계에서 웹의 정보를 검색하고 구조화하는 것에 사용되었습니다. 

(2) 대안으로는, `Invoke-WebRequest` 또는 `Invoke-RestMethod` 등을 사용할 수 있습니다. 이 두가지도 PowerShell에서 웹 페이지 다운로드에 널리 사용되는 방법입니다. 

(3) `DownloadFile` 메소드는 대상 URL에서 파일을 비동기로 다운받게 해줍니다. 이는 메인 스레드를 멈추지 않고 다운로드를 계속할 수 있게 해줍니다. 

## 참고 자료:

- PowerShell 웹 페이지 다운로드 : [링크](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7)
  
- PowerShell Invoke-WebRequest, Invoke-RestMethod: [링크](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1)
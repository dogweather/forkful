---
date: 2024-01-26 01:08:31.821530-07:00
description: "\uB85C\uADF8(Logging)\uB294 \uCF54\uB4DC\uB97C \uD1B5\uD574 \uBE44\uD589\
  \uAE30\uC758 \uADA4\uC801\uC744 \uB0A8\uAE30\uB294 \uAC83\uACFC \uAC19\uC2B5\uB2C8\
  \uB2E4 - \uC2A4\uD06C\uB9BD\uD2B8\uAC00 \uC2E4\uC81C \uD658\uACBD\uC5D0\uC11C \uC2E4\
  \uD589\uB420 \uB54C \uBB34\uC2A8 \uC77C\uC774 \uBC1C\uC0DD\uD558\uB294\uC9C0\uB97C\
  \ \uD30C\uC545\uD558\uAE30 \uC704\uD55C \uBC29\uBC95\uC785\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uB514\uBC84\uAE45\uC744 \uD558\uACE0, \uC560\uD50C\
  \uB9AC\uCF00\uC774\uC158\uC758 \uB3D9\uC791\uC744 \uCD94\uC801\uD558\uACE0, \uC131\
  \uB2A5\uC744 \uBAA8\uB2C8\uD130\uB9C1\uD558\uACE0, \uC7A0\uC7AC\uC801\uC778 \uBB38\
  \uC81C\uB97C \uAC10\uC2DC\uD558\uAE30 \uC704\uD574\u2026"
lastmod: '2024-03-11T00:14:29.480217-06:00'
model: gpt-4-1106-preview
summary: "\uB85C\uADF8(Logging)\uB294 \uCF54\uB4DC\uB97C \uD1B5\uD574 \uBE44\uD589\
  \uAE30\uC758 \uADA4\uC801\uC744 \uB0A8\uAE30\uB294 \uAC83\uACFC \uAC19\uC2B5\uB2C8\
  \uB2E4 - \uC2A4\uD06C\uB9BD\uD2B8\uAC00 \uC2E4\uC81C \uD658\uACBD\uC5D0\uC11C \uC2E4\
  \uD589\uB420 \uB54C \uBB34\uC2A8 \uC77C\uC774 \uBC1C\uC0DD\uD558\uB294\uC9C0\uB97C\
  \ \uD30C\uC545\uD558\uAE30 \uC704\uD55C \uBC29\uBC95\uC785\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uB514\uBC84\uAE45\uC744 \uD558\uACE0, \uC560\uD50C\
  \uB9AC\uCF00\uC774\uC158\uC758 \uB3D9\uC791\uC744 \uCD94\uC801\uD558\uACE0, \uC131\
  \uB2A5\uC744 \uBAA8\uB2C8\uD130\uB9C1\uD558\uACE0, \uC7A0\uC7AC\uC801\uC778 \uBB38\
  \uC81C\uB97C \uAC10\uC2DC\uD558\uAE30 \uC704\uD574\u2026"
title: "\uB85C\uAE45"
---

{{< edit_this_page >}}

## 무엇 & 왜?
로그(Logging)는 코드를 통해 비행기의 궤적을 남기는 것과 같습니다 - 스크립트가 실제 환경에서 실행될 때 무슨 일이 발생하는지를 파악하기 위한 방법입니다. 프로그래머들은 디버깅을 하고, 애플리케이션의 동작을 추적하고, 성능을 모니터링하고, 잠재적인 문제를 감시하기 위해 로그를 사용합니다.

## 사용 방법:
스크립트에 기본 로깅(log)을 추가하는 방법은 다음과 같습니다:

```PowerShell
# 간단한 로그 메시지 생성하기
Write-Host "정보: 스크립트 프로세스를 시작합니다."

# 파일로 기록하기
"정보: 이것은 로그된 메시지입니다." | Out-File -Append myLog.log

# 내장된 cmdlet을 사용하여 더 자세한 로깅하기
Start-Transcript -Path "./detailedLog.log"
Write-Output "경고: 뭔가 제대로 작동하지 않는 것 같습니다."
# ... 스크립트가 작업을 처리합니다
Stop-Transcript

# detailedLog.log의 출력
******************************
Windows PowerShell 전사 시작
시작 시간: 20230324112347
사용자 이름  : PShellGuru@example.com
권한 있는 사용자: PShellGuru@example.com
구성 이름: 
기기  : PS-DEVBOX (Microsoft Windows NT 10.0.17763.0)
호스트 응용 프로그램: C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe
프로세스 ID: 2024
PS 버전: 7.1.2
```

이제 로그에서 코드가 무엇을 했는지 일련의 기록을 볼 수 있습니다.

## 심층 탐구:
역사적으로, 로깅은 프로그래밍만큼 오래되었습니다. 그것은 소프트웨어용 선장의 로그와 같습니다. 옛날에는 프린트아웃이나 텔레타이프 기계였을 수도 있습니다; 지금은 파일과 세련된 로그 관리 시스템의 시대입니다.

파워셸의 심연에 있을 때, `Write-Host`는 빠르고 간편하지만 콘솔에 텍스트를 출력하는 것 뿐이고 기록을 남기기에는 좋지 않습니다. `Out-File`은 파일에 텍스트를 넣는 간단한 방법을 제공하지만, 실제 유용한 정보를 원한다면 모든 것을 로그하기 위해 `Start-Transcript`와 `Stop-Transcript`를 사용하길 원하게 될 것입니다.

대안이 있습니까? 물론입니다, 만약 여러분이 기업 환경에서 일한다면 Windows 이벤트 로그를 살펴보거나 Logstash 같은 소프트웨어를 사용할 수도 있겠지만, 일상적인 스크립트 작업에는 PowerShell의 도구를 사용하는 것이 좋습니다. 구현과 관련하여, 로그를 너무 적게 남기면 쓸모가 없고, 너무 많이 남기면 잡음이 되므로 현명하게 로깅하십시오.

## 참고할 것:
파워셸에서 로깅의 모든 것을 파악하기 위해 다음을 확인해보세요:

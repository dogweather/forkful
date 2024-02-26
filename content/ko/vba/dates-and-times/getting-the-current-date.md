---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:44.125433-07:00
description: "Visual Basic for Applications(VBA)\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\
  \uB97C \uAC80\uC0C9\uD558\uB294 \uAC83\uC740 \uD504\uB85C\uADF8\uB798\uBA38\uAC00\
  \ \uB9E4\uD06C\uB85C\uB098 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC5D0\uC11C \uB0A0\
  \uC9DC\uB97C \uB3D9\uC801\uC73C\uB85C \uC791\uC5C5\uD560 \uC218 \uC788\uAC8C \uD574\
  \uC8FC\uB294 \uD754\uD55C \uC791\uC5C5\uC785\uB2C8\uB2E4. \uC774 \uAE30\uB2A5\uC740\
  \ \uB85C\uAE45, \uAC70\uB798 \uD0C0\uC784\uC2A4\uD0EC\uD551 \uB610\uB294 \uB0A0\uC9DC\
  \ \uAE30\uBC18 \uACC4\uC0B0\uACFC \uAC19\uC740 \uC791\uC5C5\uC5D0 \uC911\uC694\uD569\
  \uB2C8\uB2E4."
lastmod: '2024-02-25T18:49:52.003326-07:00'
model: gpt-4-0125-preview
summary: "Visual Basic for Applications(VBA)\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\
  \uB97C \uAC80\uC0C9\uD558\uB294 \uAC83\uC740 \uD504\uB85C\uADF8\uB798\uBA38\uAC00\
  \ \uB9E4\uD06C\uB85C\uB098 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC5D0\uC11C \uB0A0\
  \uC9DC\uB97C \uB3D9\uC801\uC73C\uB85C \uC791\uC5C5\uD560 \uC218 \uC788\uAC8C \uD574\
  \uC8FC\uB294 \uD754\uD55C \uC791\uC5C5\uC785\uB2C8\uB2E4. \uC774 \uAE30\uB2A5\uC740\
  \ \uB85C\uAE45, \uAC70\uB798 \uD0C0\uC784\uC2A4\uD0EC\uD551 \uB610\uB294 \uB0A0\uC9DC\
  \ \uAE30\uBC18 \uACC4\uC0B0\uACFC \uAC19\uC740 \uC791\uC5C5\uC5D0 \uC911\uC694\uD569\
  \uB2C8\uB2E4."
title: "\uD604\uC7AC \uB0A0\uC9DC \uC5BB\uAE30"
---

{{< edit_this_page >}}

## 무엇이며 왜?

Visual Basic for Applications(VBA)에서 현재 날짜를 검색하는 것은 프로그래머가 매크로나 애플리케이션에서 날짜를 동적으로 작업할 수 있게 해주는 흔한 작업입니다. 이 기능은 로깅, 거래 타임스탬핑 또는 날짜 기반 계산과 같은 작업에 중요합니다.

## 방법:

VBA에서 현재 날짜를 검색하는 것은 `Date` 함수를 사용하는 것이 간단하며, `Now` 함수는 현재 날짜와 시간을 모두 제공합니다. 다음은 둘 다 사용하는 방법입니다:

```vb
Sub GetCurrentDate()
    ' Date 함수를 사용하여 현재 날짜를 얻기
    Dim currentDate As Date
    currentDate = Date
    Debug.Print "현재 날짜: "; currentDate
    
    ' Now 함수를 사용하여 현재 날짜와 시간을 얻기
    Dim currentDateTime As Date
    currentDateTime = Now
    Debug.Print "현재 날짜와 시간: "; currentDateTime
End Sub
```

이 매크로를 실행하면, `Debug.Print` 메서드는 VBA 편집기의 즉시 창에 현재 날짜와 현재 날짜와 시간을 출력합니다. 예를 들어:

```
현재 날짜: 4/12/2023
현재 날짜와 시간: 4/12/2023 3:45:22 PM
```

사용자 컴퓨터의 시스템 설정에 따라 날짜 형식이 다를 수 있음을 명심하세요.

## 심화

`Date` 및 `Now` 함수는 Visual Basic for Applications에서 날짜와 시간을 다루는 복잡성을 캡슐화하여, 날짜 작업을 간단하고 직관적으로 만드는 애플리케이션 수준의 추상화를 제공합니다. 역사적으로, 프로그래밍에서 날짜와 시간을 다루는 것은 다른 시간대, 일광 절약 변경 및 다양한 날짜 형식을 처리하는 등의 도전과 함께 해왔습니다.

VBA에서, 이 함수들은 기본 시스템의 날짜와 시간에 의존하며, 이는 사용자의 로케일과 시스템 설정의 영향을 받습니다. 이는 사용자 환경과의 일관성을 보장하지만, 전 세계적인 애플리케이션에서 지역화 및 시간대 조정을 신중하게 처리해야 한다는 양날의 검이 됩니다.

VBA의 날짜와 시간 함수는 특히 Office 자동화의 범위 내에서 많은 애플리케이션에 완벽하게 적합하지만, 고주파 거래 시스템이나 과학 시뮬레이션과 같은 더 복잡한 애플리케이션에 필요한 정밀도나 세밀함을 부족할 수 있습니다. 이러한 경우, 다른 프로그래밍 환경이나 Python이나 C#과 같은 언어가 더 정교한 날짜 및 시간 조작 라이브러리를 제공할 수 있습니다.

그럼에도 불구하고, Excel, Word 또는 다른 Office 애플리케이션의 맥락에서 날짜와 시간을 다루는 대부분의 작업에 대해, VBA의 `Date` 및 `Now` 함수는 간단함, 성능 및 사용의 용이성의 균형을 제공하여 이길 수 없는 선택입니다.

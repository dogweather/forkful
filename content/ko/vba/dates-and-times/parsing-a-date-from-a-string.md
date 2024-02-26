---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:45.909122-07:00
description: "Visual Basic for Applications(VBA)\uC5D0\uC11C \uBB38\uC790\uC5F4\uC5D0\
  \uC11C \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD558\uB294 \uAC83\uC740 \uB0A0\uC9DC\uB97C\
  \ \uB098\uD0C0\uB0B4\uB294 \uD14D\uC2A4\uD2B8\uB97C \uB0A0\uC9DC \uB370\uC774\uD130\
  \ \uC720\uD615\uC73C\uB85C \uBCC0\uD658\uD558\uB294 \uAC83\uC5D0 \uAD00\uD55C \uAC83\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uBE44\uAD50, \uACC4\
  \uC0B0 \uB610\uB294 \uD615\uC2DD \uC9C0\uC815 \uB4F1\uC758 \uBAA9\uC801\uC73C\uB85C\
  \ \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uBCF4\uB2E4\
  \ \uD6A8\uACFC\uC801\uC73C\uB85C\u2026"
lastmod: '2024-02-25T18:49:52.001819-07:00'
model: gpt-4-0125-preview
summary: "Visual Basic for Applications(VBA)\uC5D0\uC11C \uBB38\uC790\uC5F4\uC5D0\uC11C\
  \ \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD558\uB294 \uAC83\uC740 \uB0A0\uC9DC\uB97C \uB098\
  \uD0C0\uB0B4\uB294 \uD14D\uC2A4\uD2B8\uB97C \uB0A0\uC9DC \uB370\uC774\uD130 \uC720\
  \uD615\uC73C\uB85C \uBCC0\uD658\uD558\uB294 \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uBE44\uAD50, \uACC4\uC0B0\
  \ \uB610\uB294 \uD615\uC2DD \uC9C0\uC815 \uB4F1\uC758 \uBAA9\uC801\uC73C\uB85C \uC560\
  \uD50C\uB9AC\uCF00\uC774\uC158\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uBCF4\uB2E4 \uD6A8\
  \uACFC\uC801\uC73C\uB85C\u2026"
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇이며 왜?

Visual Basic for Applications(VBA)에서 문자열에서 날짜를 파싱하는 것은 날짜를 나타내는 텍스트를 날짜 데이터 유형으로 변환하는 것에 관한 것입니다. 프로그래머들은 비교, 계산 또는 형식 지정 등의 목적으로 애플리케이션에서 날짜를 보다 효과적으로 조작하기 위해 이 작업을 수행합니다.

## 방법:

VBA는 `CDate` 함수나 `DateValue` 함수를 사용하여 문자열을 날짜로 파싱하는 간단한 방법을 제공합니다. 그러나 문자열이 인식 가능한 날짜 형식인지 확인하는 것이 중요합니다.

`CDate` 사용한 기본 예시는 다음과 같습니다:

```basic
Sub ParseDateUsingCDate()
    Dim dateString As String
    Dim parsedDate As Date
    
    dateString = "2023-04-01"
    parsedDate = CDate(dateString)
    
    Debug.Print "Parsed Date: "; parsedDate
End Sub
```

이 코드를 실행하면 VBA 편집기에서 `Ctrl+G`로 접근 가능한 즉시 창(Immediate Window)에서 출력은 다음과 같습니다:

```
Parsed Date: 4/1/2023 
```

대안으로, 시간 부분을 무시하는 데 더 특화된 `DateValue` 함수를 사용할 수 있습니다:

```basic
Sub ParseDateUsingDateValue()
    Dim dateString As String
    Dim parsedDate As Date
    
    dateString = "April 1, 2023"
    parsedDate = DateValue(dateString)
    
    Debug.Print "Parsed Date using DateValue: "; parsedDate
End Sub
```

이에 대한 샘플 출력도 즉시 창에서 비슷하게 표시됩니다:

```
Parsed Date using DateValue: 4/1/2023
```

문자열의 날짜 형식이 시스템 또는 애플리케이션 설정과 일치해야 파싱이 성공한다는 것을 기억하세요.

## 심층 분석

VBA가 문자열을 날짜로 파싱할 때 내부적으로 Windows 운영 시스템의 지역 설정을 사용하여 날짜 형식을 해석합니다. 이는 서로 다른 날짜/시간 설정을 사용하는 경우 하나의 시스템에서 완벽하게 파싱된 날짜 문자열이 다른 시스템에서 오류를 일으킬 수 있기 때문에 이해하는 것이 중요합니다.

역사적으로 날짜 처리는 국제적으로 사용되는 애플리케이션에서 흔한 버그의 원인이 되어왔습니다. VBA에서 지역 설정에 대한 이러한 의존성은 다른 시스템에서의 모호함 없는 날짜 표현 및 파싱을 위해 ISO 8601 형식(예: "YYYY-MM-DD")과 같은 대안을 고려할 수 있는 이유입니다. 불행히도 VBA는 기본적으로 ISO 8601을 지원하지 않으며 엄격한 준수를 위해 수동 파싱이 필요합니다.

`CDate`나 `DateValue`가 처리할 수 있는 것을 넘어서는 복잡한 날짜 파싱이나 시스템 로케일 설정에 관계없이 일관된 파싱을 보장하기 위해, 프로그래머는 사용자 정의 파싱 함수로 돌아갈 수 있습니다. 이러한 함수는 날짜 문자열을 컴포넌트 (년, 월, 일)로 분리하고 `DateSerial` 함수를 사용해 날짜를 구성하는 작업을 포함할 수 있습니다. 또한 이러한 작업을 위해 국제화를 염두에 둔 더 강력한 언어나 라이브러리를 선택할 수도 있습니다.

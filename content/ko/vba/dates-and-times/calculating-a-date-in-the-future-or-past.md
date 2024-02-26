---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:49:32.154302-07:00
description: "\uBBF8\uB798 \uB610\uB294 \uACFC\uAC70\uC758 \uB0A0\uC9DC\uB97C \uACC4\
  \uC0B0\uD558\uB294 \uAC83\uC740 \uC8FC\uC5B4\uC9C4 \uB0A0\uC9DC\uB85C\uBD80\uD130\
  \ \uD2B9\uC815 \uC77C\uC218, \uC6D4\uC218, \uB610\uB294 \uB144\uC218\uB9CC\uD07C\
  \ \uB5A8\uC5B4\uC9C4 \uB0A0\uC9DC\uB97C \uACB0\uC815\uD558\uB294 \uAC83\uC744 \uD3EC\
  \uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uC885\uC885 \uC54C\
  \uB9BC, \uAD6C\uB3C5, \uB9CC\uB8CC \uB0A0\uC9DC, \uADF8\uB9AC\uACE0 \uB2E4\uC591\
  \uD55C \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC5D0\uC11C \uC77C\uC815 \uC791\uC5C5\
  \uC744 \uC790\uB3D9\uD654\uD558\uAE30 \uC704\uD574 \uC774 \uAE30\uB2A5\uC774 \uD544\
  \uC694\uD569\uB2C8\uB2E4."
lastmod: '2024-02-25T18:49:52.008391-07:00'
model: gpt-4-0125-preview
summary: "\uBBF8\uB798 \uB610\uB294 \uACFC\uAC70\uC758 \uB0A0\uC9DC\uB97C \uACC4\uC0B0\
  \uD558\uB294 \uAC83\uC740 \uC8FC\uC5B4\uC9C4 \uB0A0\uC9DC\uB85C\uBD80\uD130 \uD2B9\
  \uC815 \uC77C\uC218, \uC6D4\uC218, \uB610\uB294 \uB144\uC218\uB9CC\uD07C \uB5A8\uC5B4\
  \uC9C4 \uB0A0\uC9DC\uB97C \uACB0\uC815\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uC885\uC885 \uC54C\uB9BC, \uAD6C\
  \uB3C5, \uB9CC\uB8CC \uB0A0\uC9DC, \uADF8\uB9AC\uACE0 \uB2E4\uC591\uD55C \uC560\uD50C\
  \uB9AC\uCF00\uC774\uC158\uC5D0\uC11C \uC77C\uC815 \uC791\uC5C5\uC744 \uC790\uB3D9\
  \uD654\uD558\uAE30 \uC704\uD574 \uC774 \uAE30\uB2A5\uC774 \uD544\uC694\uD569\uB2C8\
  \uB2E4."
title: "\uBBF8\uB798 \uB610\uB294 \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\uD558\
  \uAE30"
---

{{< edit_this_page >}}

## 무엇이며 왜인가?
미래 또는 과거의 날짜를 계산하는 것은 주어진 날짜로부터 특정 일수, 월수, 또는 년수만큼 떨어진 날짜를 결정하는 것을 포함합니다. 프로그래머는 종종 알림, 구독, 만료 날짜, 그리고 다양한 애플리케이션에서 일정 작업을 자동화하기 위해 이 기능이 필요합니다.

## 어떻게:
Visual Basic for Applications(VBA)에서 미래 또는 과거 날짜를 계산하는 데 사용되는 주요 함수는 `DateAdd()`입니다. 이 함수는 특정 시간 간격을 날짜에 더하여 새로운 날짜를 반환합니다.

현재 날짜에 10일을 더하는 기본 예제입니다:

```vb
Dim futureDate As Date
futureDate = DateAdd("d", 10, Date) ' 현재 날짜에 10일을 더함
Debug.Print futureDate ' 출력 올 것임: 04/20/2023
```

마찬가지로, 과거의 날짜를 10일 찾기:

```vb
Dim pastDate As Date
pastDate = DateAdd("d", -10, Date) ' 현재 날짜에서 10일을 뺌
Debug.Print pastDate ' 출력: 03/31/2023, 오늘이 04/10/2023라고 가정할 때
```

이 예제들은 매우 직관적입니다. `"d"`를 다른 간격 코드로 대체할 수 있으며, 예를 들어 `"m"`은 월, `"yyyy"`는 년을 의미하여 다른 유형의 날짜 계산을 수행할 수 있습니다. 여기 미래의 날짜를 1년 계산하는 방법이 있습니다:

```vb
Dim nextYear As Date
nextYear = DateAdd("yyyy", 1, Date) ' 현재 날짜에 1년을 더함
Debug.Print nextYear ' 출력: 04/10/2024, 오늘이 04/10/2023일 경우
```

## 심층 분석
`DateAdd` 함수는 VBA가 시작될 때부터 그것의 전신이었던 BASIC에서 유래하여, VBA의 기본적인 부분이 되었습니다. 이 함수는 날짜에서 시간 간격을 더하거나 빼는 것을 단순화하지만, VBA를 포함한 그 날짜 처리 함수들이 새로운 프로그래밍 언어에서 발견되는 편리성이나 효율성과 항상 일치하지는 않는다는 점을 알아두는 것이 중요합니다.

예를 들어, `datetime` 모듈을 가진 Python이나 `moment.js` 및 `date-fns`와 같은 라이브러리를 가진 JavaScript는 날짜 조작을 위한 더 직관적이고 강력한 방법을 제공합니다. 이러한 옵션은 지역화, 시간대, 윤년에 대한 더 나은 지원을 제공하여 정확한 날짜 계산이 필요한 전 세계적 규모의 애플리케이션에 더 적합하게 만듭니다.

그러나 Excel 매크로와 Microsoft Office 생태계 내에서 통합이 필요한 애플리케이션의 경우, VBA는 실용적인 선택으로 남아 있습니다. Excel 데이터에 직접 액세스하고 조작하는 것의 단순성은 상당한 장점입니다. 또한, 일정 작업과 알림과 같은 대부분의 기본적인 날짜 계산을 위해, VBA의 `DateAdd()`는 적절하고 간단한 해결책을 제공합니다. 그 문법은 초보자에게 쉽게 이해할 수 있으며, 더 넓은 Office 스위트 애플리케이션과의 통합은 특정 사용 사례에서 그것의 관련성을 보장합니다.

결론적으로, 대체 프로그램 언어가 날짜 계산에 대한 더 현대적인 접근 방식을 제공할 수 있지만, VBA의 `DateAdd()`는 필요한 분야에서 그 언어의 지속적인 힘을 증명하는 것입니다.

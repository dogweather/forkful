---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:43.656132-07:00
description: "\uBC29\uBC95: VBA\uC5D0\uC11C \uB514\uBC84\uAC70\uB294 Visual Basic\
  \ \uD3B8\uC9D1\uAE30(VBE)\uC5D0 \uD544\uC218\uC801\uC778 \uBD80\uBD84\uC785\uB2C8\
  \uB2E4. \uC5EC\uAE30\uC11C\uB294 \uB514\uBC84\uAC70\uB97C \uD65C\uC6A9\uD558\uB294\
  \ \uBC29\uBC95\uC744 \uC54C\uC544\uBD05\uB2C8\uB2E4: 1. **\uC911\uB2E8\uC810 \uC124\
  \uC815**: \uAD00\uC2EC \uC788\uB294 \uCF54\uB4DC \uC904 \uC606\uC5D0 \uC788\uB294\
  \ \uC67C\uCABD \uC5EC\uBC31\uC744 \uD074\uB9AD\uD558\uAC70\uB098 \uCEE4\uC11C\uB97C\
  \ \uD574\uB2F9 \uC904\uC5D0 \uB450\uACE0 F9\uB97C \uB204\uB974\uC2ED\uC2DC\uC624\
  . \uC774\uB294 VBA\uC5D0 \uC774\u2026"
lastmod: '2024-03-13T22:44:54.988467-06:00'
model: gpt-4-0125-preview
summary: "VBA\uC5D0\uC11C \uB514\uBC84\uAC70\uB294 Visual Basic \uD3B8\uC9D1\uAE30\
  (VBE)\uC5D0 \uD544\uC218\uC801\uC778 \uBD80\uBD84\uC785\uB2C8\uB2E4."
title: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uD558\uAE30"
weight: 35
---

## 방법:
VBA에서 디버거는 Visual Basic 편집기(VBE)에 필수적인 부분입니다. 여기서는 디버거를 활용하는 방법을 알아봅니다:

1. **중단점 설정**: 관심 있는 코드 줄 옆에 있는 왼쪽 여백을 클릭하거나 커서를 해당 줄에 두고 F9를 누르십시오. 이는 VBA에 이 지점에 도달했을 때 실행을 일시 중지하도록 알립니다.

    ```vb
    Sub DebugExample()
        Dim counter As Integer
        For counter = 1 To 5
            Debug.Print counter ' 여기에 중단점 설정
        Next counter
    End Sub
    ```

    코드가 실행되면, `Debug.Print counter` 줄에서 일시 중지되어 변수 값을 검사할 수 있습니다.

2. **단계 진입(F8)**: 이 명령을 사용하면 코드를 한 문장씩 실행하며, 호출된 모든 절차에 진입합니다. 코드와 함수가 어떻게 상호작용하는지 추적하는 데 유용합니다.

3. **감시 창**: 감시 창을 사용하면 변수나 표현식의 값을 모니터링할 수 있습니다. 변수가 범위 내에 없으면 감시 창이 이를 표시합니다. 변수를 마우스 오른쪽 버튼으로 클릭 > 감시 추가.

4. **즉시 창 (Ctrl+G)**: 이 창은 디버깅하는 동안 표현식을 테스트하거나 변수 값들을 수정하기에 특히 유용합니다. `?변수명`을 입력하여 변수의 현재 값을 출력하거나 `변수명 = 새값`으로 새 값을 할당합니다.

    ```vb
    ' 즉시 창에서
    ?counter ' counter의 현재 값을 출력함
    counter = 3 ' counter 값을 3으로 설정함
    ```

5. **샘플 출력**:

    중단점에 도착하여 F8을 사용하여 한 줄씩 실행할 때, 즉시 창에는 다음과 같은 내용이 표시될 수 있습니다:

    ```
    counter = 1
    counter = 2
    counter = 3
    ```

    여기서는 반복할 때마다 `counter` 변수를 수동으로 쿼리했습니다.

## 깊게 살펴보기:
VBA의 디버거는 견고하면서도 프로그래밍 언어에서 디버깅 도구의 더 넓은 전통의 한 부분입니다. VBA의 초기 버전과 함께 소개된 이 디버거는 개발자들에게 코드 검사와 수정을 위한 간단하면서도 강력한 도구 세트를 제공하고자 했습니다. 시간이 지나면서 조건부 중단점, 개선된 감시 기능, Excel 인터페이스와의 통합 등의 향상된 기능이 포함되었습니다.

그러나 Visual Studio 또는 Eclipse와 같은 현대적인 통합 개발 환경(IDE)에 비해 VBA의 디버깅 도구는 기본적으로 보일 수 있습니다. 이러한 현대 IDE는 실시간 변수 검사, 고급 중단점 및 통합 유닛 테스팅 프레임워크와 같은 더 정교한 기능을 제공합니다. 이러한 대안이 보다 포괄적인 디버깅 경험을 제공하더라도, VBA 디버거의 단순성과 직접성은 Microsoft Office 응용 프로그램 내에서 자동화 및 스크립팅의 특정 맥락에 잘 맞습니다.

이러한 현대 환경에 익숙한 프로그래머는 VBA의 디버깅 도구에 적응하려면 접근 방식을 바꿔야 할 수 있습니다. 그러나 변수 검사, 코드 단계별 실행, 런타임 행동 관찰의 근본적인 원칙은 보편적입니다. 연습을 통해, VBA의 디버거는 Office 생태계 내에서 자동화 스크립트가 완벽하게 수행되도록 하는 데 없어서는 안 될 도구가 됩니다.

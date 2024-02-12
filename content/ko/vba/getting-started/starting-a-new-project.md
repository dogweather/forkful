---
title:                "새 프로젝트 시작하기"
aliases:
- /ko/vba/starting-a-new-project.md
date:                  2024-02-01T22:03:07.075859-07:00
model:                 gpt-4-0125-preview
simple_title:         "새 프로젝트 시작하기"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/vba/starting-a-new-project.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

Visual Basic for Applications(VBA)에서 새 프로젝트를 시작한다는 것은 Excel과 같은 호스트 애플리케이션 내에 환경을 설정하여 작업을 자동화하거나 기능을 확장하는 것을 포함합니다. 프로그래머들은 Microsoft Office 애플리케이션을 사용자 정의하고 자동화하여 워크플로우를 간소화하고 생산성을 향상시키기 위해 VBA의 힘을 활용하기 위해 이 분야로 진출합니다.

## 방법:

새로운 VBA 프로젝트를 시작할 준비가 되었다면, 보통의 시작점은 VBA 편집기에 접근하고 프로젝트 프레임워크를 초기화하는 것을 포함합니다. Excel을 호스트 애플리케이션으로 사용하여 단계를 따라 해보겠습니다:

1. **VBA 편집기 열기**: Excel에서 `Alt + F11`을 눌러 VBA 편집기에 접근합니다.
2. **새 모듈 삽입**: 메뉴에서 `삽입 > 모듈`로 이동하여 프로젝트에 새 모듈을 추가합니다. 여기에 코드가 위치하게 됩니다.
3. **첫 번째 매크로 작성**: 메시지 박스를 표시하는 간단한 매크로를 코딩해봅시다. 모듈 안에 다음 코드를 입력하세요:

```vb
Sub SayHello()
    MsgBox "Hello, World!", vbInformation, "인사"
End Sub
```

4. **매크로 실행하기**: `SayHello` 서브 안에 커서가 있을 때 `F5`를 누르거나 `실행 > Run Sub/UserForm`으로 가서 `SayHello`를 선택합니다. "Hello, World!"와 "확인" 버튼이 있는 메시지 박스가 팝업되는 것을 볼 수 있습니다.

샘플 출력:

```plaintext
"Hello, World!"가 표시된 메시지 박스.
```

5. **프로젝트 저장하기**: 종료하기 전에 작업을 저장했는지 확인하세요. Excel 워크북이 이전에 저장되지 않았다면 매크로 활성화 워크북(`.xlsm` 파일 형식)으로 저장하라는 메시지가 표시됩니다.

## 심층 분석

Visual Basic for Applications는 1993년 도입 이후 Microsoft 자동화 전략의 핵심이 되었습니다. 이전 버전인 MacroBasic의 진화로 시작된 VBA는 Microsoft Office 제품군 전반에 걸쳐 향상된 통합 기능을 제공하는 보다 강력한 솔루션을 제공했습니다. VBA로의 전환은 보다 복잡한 스크립팅 기능을 향한 전환이었으며, 완전한 프로그래밍 언어의 힘을 활용할 수 있게 됨을 표시했습니다.

그 나이에도 불구하고 VBA는 Office 제품과의 깊은 통합과 많은 조직에서의 기존 코드 대량 보유로 인해 현대 사무 환경에서 여전히 널리 사용되고 있습니다. 하지만, 새로운 웹 기반 애플리케이션에 대해 또는 Office 애플리케이션 외부와의 통합이나 확장성이 더 필요한 작업을 위해, Python과 같은 풍부한 라이브러리 생태계를 가진 언어, 또는 Office 스크립트를 위한 JavaScript와 같은 언어와 프레임워크가 더 현대적이고 다재다능한 접근 방식을 제공합니다. 이러한 대안들은 배우기가 더 어렵고 설정이 필요하지만, 버전 관리 및 배포 파이프라인과 같은 현대적 개발 관행을 위한 보다 넓은 적용 가능성과 지원을 제공합니다.

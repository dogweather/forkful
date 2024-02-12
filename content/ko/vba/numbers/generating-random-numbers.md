---
title:                "난수 생성하기"
aliases:
- /ko/vba/generating-random-numbers.md
date:                  2024-02-01T21:54:25.283571-07:00
model:                 gpt-4-0125-preview
simple_title:         "난수 생성하기"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/vba/generating-random-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

Visual Basic for Applications(VBA)에서 난수를 생성하는 것은 주사위 굴림이나 데이터 샘플링과 같이 우연성 또는 변동성 요소가 있는 프로세스를 시뮬레이션하는 프로그램을 가능하게 합니다. 프로그래머들은 예측 가능한 결과가 비현실적이거나 덜 유용할 수 있는 모델, 게임, 또는 시뮬레이션을 개발하기 위해 이러한 기술을 사용합니다.

## 방법:

VBA에서는 `Rnd` 함수를 사용하여 난수를 생성합니다. 기본적으로, `Rnd`는 0 이상 1 미만의 단정밀도 부동 소수점 숫자를 생성합니다. 다음은 난수를 효과적으로 활용하기 위한 몇 가지 단계 및 예시입니다:

1. **단순 난수 생성:**
   기본적인 난수를 생성하려면 `Rnd()`를 호출하기만 하면 됩니다:

   ```vb
   Sub GenerateRandomNumber()
       Dim randomNumber As Single
       randomNumber = Rnd() ' 0과 1 사이의 난수
       MsgBox randomNumber
   End Sub
   ```

2. **시드 설정:**
   `Randomize` 문은 난수 생성기를 초기화하는데, 이는 VBA 코드가 실행될 때마다 다른 결과를 보장하는 데 중요할 수 있습니다:

   ```vb
   Sub SeedRandomNumber()
       Randomize
       Dim randomNumber As Single
       randomNumber = Rnd()
       MsgBox randomNumber
   End Sub
   ```

3. **범위 내에서 난수 생성:**
   종종 특정 범위 내의 난수가 필요할 것입니다. 여기서는 1부터 100 사이의 숫자를 생성하는 방법입니다:

   ```vb
   Sub RandomNumberInRange()
       Randomize
       Dim randomNumber As Integer
       randomNumber = Int((100 * Rnd()) + 1) ' 1과 100 사이의 난수
       MsgBox randomNumber
   End Sub
   ```

### 샘플 출력:
`RandomNumberInRange`를 실행한 후에는, 메시지 박스에 `45`와 같은 숫자가 표시될 수 있습니다.

## 심층 탐구:

VBA의 `Rnd` 함수는 사용하기 쉽지만, 결정적 알고리즘에 기반한 의사 난수를 실제로 생성합니다. 이는 생성된 숫자 시퀀스가 진정으로 무작위적이지 않지만, 확률 과정이 필요한 일반적인 작업에는 종종 충분하다는 의미입니다.

역사적으로, VBA에서의 난수 생성 능력은 Basic의 초기 버전으로 거슬러 올라가며, 시간이 지남에 따라 알고리즘에 시작점을 시딩하여 무작위성을 향상시키는 `Randomize`와 같은 기능을 포함하도록 발전했습니다. 그러나 보안 암호화 작업과 같이 고도의 무작위성이 요구되는 응용 프로그램의 경우, VBA의 `Rnd`는 최상의 도구가 아닐 수 있습니다. Python의 `secrets` 모듈이나 Java의 `SecureRandom`과 같이 암호학을 염두에 둔 보다 강력한 프로그래밍 환경 또는 언어에서 고려해야 할 대안이 있습니다.

그럼에도 불구하고, VBA에서 난수를 생성하는 것의 단순성과 접근성은 다양한 경량 애플리케이션, 시뮬레이션 작업 및 교육 목적에 대해 계속해서 귀중한 도구로 만듭니다.

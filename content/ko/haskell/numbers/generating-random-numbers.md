---
date: 2024-01-27 20:33:56.346802-07:00
description: "\uBC29\uBC95: Haskell\uC5D0\uC11C \uB09C\uC218\uB97C \uC0DD\uC131\uD558\
  \uAE30 \uC704\uD574 \uC77C\uBC18\uC801\uC73C\uB85C Haskell Platform\uC758 \uC77C\
  \uBD80\uC778 `random` \uD328\uD0A4\uC9C0\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uB2E4\
  \uC74C\uC740 \uB2E8\uACC4\uBCC4 \uAC00\uC774\uB4DC\uC785\uB2C8\uB2E4: \uBA3C\uC800\
  , `random` \uD328\uD0A4\uC9C0\uAC00 \uC124\uCE58\uB418\uC5B4 \uC788\uB294\uC9C0\
  \ \uD655\uC778\uD558\uC138\uC694. \uC124\uCE58\uB418\uC5B4 \uC788\uC9C0 \uC54A\uB2E4\
  \uBA74, Cabal\uC774\uB098 Stack\uC744\u2026"
lastmod: '2024-03-13T22:44:55.289696-06:00'
model: gpt-4-0125-preview
summary: "Haskell\uC5D0\uC11C \uB09C\uC218\uB97C \uC0DD\uC131\uD558\uAE30 \uC704\uD574\
  \ \uC77C\uBC18\uC801\uC73C\uB85C Haskell Platform\uC758 \uC77C\uBD80\uC778 `random`\
  \ \uD328\uD0A4\uC9C0\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uB09C\uC218 \uC0DD\uC131"
weight: 12
---

## 방법:
Haskell에서 난수를 생성하기 위해 일반적으로 Haskell Platform의 일부인 `random` 패키지를 사용합니다. 다음은 단계별 가이드입니다:

먼저, `random` 패키지가 설치되어 있는지 확인하세요. 설치되어 있지 않다면, Cabal이나 Stack을 통해 설치할 수 있습니다.

### 난수 생성하기
간단한 난수를 생성하기 위해, 지정된 범위 내에서 난수 값을 생성하는 `randomRIO` 함수를 사용할 수 있습니다.

```Haskell
import System.Random (randomRIO)

main :: IO ()
main = do
  randomNumber <- randomRIO (1, 10) :: IO Int
  putStrLn $ "Random number: " ++ show randomNumber
```

### 난수 리스트 생성하기
난수 리스트를 생성하는 것은 약간 더 복잡하지만 여전히 간단합니다:

```Haskell
import System.Random (randomRIO)

randomList :: Int -> IO [Int]
randomList 0 = return []
randomList n = do
  r <- randomRIO (1, 100)
  rs <- randomList (n-1)
  return (r:rs)

main :: IO ()
main = do
  numbers <- randomList 5
  print numbers
```

이 코드 스니펫은 난수 정수 리스트를 생성하는 `randomList` 함수를 만듭니다. 원하는 범위로 `(1, 100)`을 교체하세요.

## 심층 탐구
Haskell `random` 패키지는 의사 난수 생성기(PRNG)를 제공하는데, 이는 생성된 숫자들이 진정으로 무작위가 아니지만 많은 응용 프로그램에 대해 무작위로 보일 수 있다는 것을 의미합니다. Haskell의 난수 생성 능력의 핵심은 난수를 생성하는 다양한 방법을 추상화하는 `RandomGen` 타입 클래스와 무작위로 생성될 수 있는 타입을 포함하는 `Random` 타입 클래스에 있습니다.

역사적으로, Haskell의 난수 생성 접근 방식은 순수성과 재현성을 강조해 왔습니다. 이것이 바로 랜덤성을 다루는 작업이 `IO` 모나드에서 명시적으로 처리되거나, 참조 투명성을 유지하기 위해 수동으로 생성기 상태를 전달하고 업데이트하는 것이 필요한 이유입니다.

특정 응용 프로그램에서는, 기본 PRNG에 의해 생성된 의사 난수가 충분히 안전하지 않을 수 있습니다. 이러한 사용 사례의 경우, Haskell 프로그래머들은 암호화 응용 프로그램의 엄격한 요구 사항을 충족하도록 설계된 `crypto-random`과 같은 더 전문화된 라이브러리로 종종 전환합니다.

또한, `mwc-random`과 같은 대안 라이브러리는 메르센 트위스터와 같은 현대 알고리즘을 구현함으로써 시뮬레이션 및 기타 응용 프로그램에 대한 난수의 품질과 성능을 향상시킵니다.

Haskell에서 난수 생성 방식을 선택할 때에는 난수의 품질, 성능 및 보안에 대한 응용 프로그램의 요구 사항을 고려하여 가장 적절한 도구나 라이브러리를 선택하는 것이 중요합니다.

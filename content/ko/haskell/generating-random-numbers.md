---
title:                "난수 생성하기"
date:                  2024-01-20T17:49:14.961067-07:00
model:                 gpt-4-1106-preview
simple_title:         "난수 생성하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
랜덤 숫자를 생성하는 것은 예측할 수 없는 숫자를 만드는 과정입니다. 프로그래머들은 게임, 시뮬레이션, 보안 알고리즘 등에서 요소의 불확실성을 추가하기 위해 이를 사용합니다.

## How to: (방법)
```Haskell
import System.Random

main :: IO ()
main = do
    gen <- getStdGen
    print . take 5 $ (randomRs (1,100) gen :: [Int])
```
샘플 출력:
```
[23, 42, 95, 56, 78]
```

## Deep Dive (심층 분석)
랜덤 숫자 생성은 프로그래밍에서 중요한 역할을 해왔습니다. Haskell에서는 `System.Random` 모듈을 사용하는 것이 기본적인 방법입니다. `randomRs` 함수는 지정한 범위 내의 무작위 숫자 리스트를 만듭니다. 경쟁하는 라이브러리로 `random-fu`나 `mwc-random`이 있으며, 성능이나 특정 요구 사항에 따라 사용됩니다. 이러한 라이브러리들은 내부적으로 다양한 난수 발생 알고리즘을 사용하여 랜덤 시퀀스를 생성합니다.

## See Also (관련 링크)
- Haskell Random: https://hackage.haskell.org/package/random
- System.Random documentation: https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html
- `random-fu` package: https://hackage.haskell.org/package/random-fu
- `mwc-random` package: https://hackage.haskell.org/package/mwc-random
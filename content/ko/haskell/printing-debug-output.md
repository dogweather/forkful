---
title:                "Haskell: 디버그 출력 출력"
programming_language: "Haskell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

# 왜 디버그 출력을 제공하는가?

디버그 출력은 우리가 코드를 디버깅하는 동안 유용한 정보를 제공해줍니다. 코드의 실행 중에 어떤 값이 할당되는지, 조건문이 어떻게 평가되는지, 함수가 어떻게 호출되는지 등을 알 수 있습니다. 이를 통해 버그를 찾고 해결하는 데 도움이 됩니다.

## 해당 기능을 사용하는 방법

디버그 출력은 Haskell을 포함한 다른 프로그래밍 언어에서 모두 사용할 수 있습니다. Haskell에서 디버그 출력을 생성하는 가장 일반적인 방법은 `Debug.Trace` 모듈을 사용하는 것입니다.

아래의 예제 코드는 `Debug.Trace` 모듈을 사용하여 디버그 출력을 생성하는 방법을 보여줍니다.

```Haskell
import Debug.Trace

main :: IO()
main = do
  let x = 5      -- 변수 x에 5 할당
  putStrLn "디버그 출력 시작"
  traceShow x $ do
    -- 이 내부에서 x의 값을 사용하여 어떤 계산 수행
    let result = x * x + 2
    putStrLn $ "결과값: " ++ show result
  putStrLn "디버그 출력 끝"
```

위의 코드를 실행하면 다음과 같은 출력 결과를 얻을 수 있습니다.

```
디버그 출력 시작
x: 5
결과값: 27
디버그 출력 끝
```

위의 예제에서는 `traceShow` 함수를 사용하여 `x`의 값을 출력하고 계산 결과를 확인할 수 있습니다. 또한 여러 줄의 코드를 디버그 출력으로 포함하려면 `trace` 함수를 사용하면 됩니다.

## 디버그 출력의 깊은 곳

디버그 출력의 깊은 곳을 파헤쳐보면 더 많은 기능을 발견할 수 있습니다. 예를 들어, `Debug.Trace` 모듈에는 `traceStack` 함수가 있습니다. 이 함수를 사용하면 현재 함수의 콜 스택을 출력할 수 있어서 어떤 함수가 어떤 함수를 호출하는지 확인할 수 있습니다.

또한 디버그 출력을 사용하여 어떤 함수가 어떤 인자를 받는지 확인할 수도 있습니다. `traceId` 함수는 인자를 출력하지 않고 그대로 반환하여 이를 가능하게 합니다.

더 많은 디버그 출력 기능을 알아보려면 `Debug.Trace` 모듈의 문서를 참조해보세요.

# 더 알아보기

디버그 출력을 활용하는 데 더 많은 정보를 찾고 싶다면 아래의 링크들을 참고해보세요.

- [Haskell 공식 문서 - Debug.Trace](https://hackage.haskell.org/package/base-4.15.0.0/docs/Debug-Trace.html)
- [Haskell Wiki - Debugging](https://wiki.haskell.org/Debugging)
- [Haskell Tutorial - Debugging Haskell Code](http://learnyouahaskell.com/debugging-haskell-code) 

# 관련 링크

- [디버깅을 위한 여러 가지 기술](https://okky.kr/article/419598)
- [함수형 프로그래밍에서 디버깅하기](http://www.kocdump.com/hl/understanding-fp-debugging)
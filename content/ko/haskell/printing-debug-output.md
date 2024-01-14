---
title:    "Haskell: 디버그 출력하기"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

디버그 출력을 하는 이유는 무엇일까요? 프로그램을 작성할 때 걸리는 시간과 노력을 절약할 수 있습니다.

## 하는 법

아래의 코드 블록을 보고 디버그 출력을 하는 방법을 익혀보세요!

```Haskell
-- 기초 예제
x = 5
y = 10
z = x + y
putStrLn $ "x의 값은 " ++ show x ++ "입니다."
putStrLn $ "y의 값은 " ++ show y ++ "입니다."
putStrLn $ "x와 y를 더한 값은 " ++ show z ++ "입니다."
```

## 깊게 들어가보기

디버그 출력에 대해 더 자세한 정보를 알고 싶다면 아래의 링크를 참고하세요. 이는 더 많은 디버깅 기술을 익힐 수 있도록 도움이 될 것입니다.

### 링크:

- [Haskell 디버깅 기술](https://www.haskell.org/haskellwiki/Debugging)
- [Haskell 디버깅을 위한 GHC 디버거 사용하기](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugger.html)
- [Haskell의 디버그 출력 및 디버거 기능 사용하기](https://www.fpcomplete.com/blog/2018/02/debugging-haskell/) 

## 자세히 알아보기

- [Haskell 디버깅에 대한 좋은 접근 방법](https://taylor.fausak.me/2014/03/05/good-approach-to-debugging-in-haskell/)
- [Haskell의 디버그 출력을 남기는 방법](https://www.linuxjournal.com/content/practical-haskell-debugging-techniques)
- [Haskell의 디버깅: 명령형 디버거의 대안](https://www.fpcomplete.com/blog/2011/10/haskell-no-debugger-no-problem-introducing-the-trace-monad) 

## 관련 자료

- [Markdown 사용법](https://www.markdownguide.org/basic-syntax/)
- [Haskell 공식 홈페이지](https://www.haskell.org/)
---
title:    "Haskell: 표준 에러에 쓰기"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜
이 블로그 글에서는 "표준 오류(STDERR)에 쓰기"에 대해 이야기하려고 합니다. Haskell에서 표준 오류를 쓸 때 사용하는 방법과 깊이 파고들어 표준 오류를 더 잘 이해할 수 있는 정보를 제공하고자 합니다.

## 방법
우선, Haskell 컴파일러를 사용하여 "```Haskell putStrLn "Hello, World!" ```" 코드를 작성합니다. 그리고 "ghc" 커맨드로 컴파일한 다음 실행하면 표준 출력(STDOUT)을 볼 수 있습니다. 하지만, 표준 오류(STDERR)를 보기 위해서는 "```Haskell putStrLn "Hello, World!" ```" 코드를 "hPutStrLn stderr"로 바꿔주면 됩니다. 아래는 예시 코드와 실행 결과입니다.

```Haskell
main = do
  hPutStrLn stderr "에러 메세지"
  putStrLn "성공 메세지"
```

실행 결과:

```
에러 메세지
성공 메세지
```

## 깊이 파고들기
표준 오류를 사용하는 이유는 프로그램의 디버깅을 쉽게하기 위해서입니다. 프로그램이 정상적으로 작동하지 않을 때, 표준 출력으로는 어떤 오류가 발생했는지 알 수 없지만, 표준 오류를 사용하면 오류 메세지를 통해 어떤 부분에서 문제가 발생했는지 쉽게 파악할 수 있습니다.

또한, 표준 오류를 사용하면 프로그램의 실행 결과와는 별개로 오류 메세지를 따로 저장하거나 다른 프로그램으로 처리하는 일이 가능해집니다. 이를 통해 오류를 보다 적극적으로 관리할 수 있게 됩니다.

## 참고자료
- [Haskell 공식 문서 – 표준 입출력](https://wiki.haskell.org/IO)
- [Haskell 예제로 배우는 함수형 프로그래밍 – 입출력](https://en.wikibooks.org/wiki/Haskell/IO)
- [모나드와 입력/출력 (Monad and Input/Output)](https://en.wikibooks.org/wiki/Haskell/Understanding_monads/IO)    

## 참고문헌
- [Markdown 한국어 문서](https://ko.wikipedia.org/wiki/마크다운)
- [Haskell 한국어 문서](https://ko.wikipedia.org/wiki/하스켈)
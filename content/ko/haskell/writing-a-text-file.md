---
title:    "Haskell: 텍스트 파일 작성하기"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 작성하는 이유는 프로그래밍에서 중요한 작업입니다. 이를 통해 데이터를 저장하고 다른 프로그램에서 사용할 수 있습니다.

## 작성 방법

먼저, 텍스트 파일을 작성하기 위해 어떤 프로그래밍 언어를 사용할지 결정해야 합니다. 이 글에서는 하스켈을 사용할 것입니다. 하스켈에서는 텍스트 파일을 쉽게 작성할 수 있도록 Text 모듈을 제공합니다. 이 모듈을 사용하여 다음과 같이 간단하게 텍스트 파일을 작성할 수 있습니다.

```Haskell
import System.IO

main = do
  file <- openFile "example.txt" WriteMode
  hPutStrLn file "안녕하세요!"
  hClose file
```

위의 코드를 실행하면 "example.txt"라는 파일이 생성되고, 그 안에 "안녕하세요!"라는 내용이 적힌 것을 확인할 수 있습니다.

## 깊이있는 공부

텍스트 파일을 작성할 때 주의해야 할 점이 많이 있습니다. 예를 들어, 파일을 열기 전에 먼저 파일의 존재 여부를 확인하는 것이 좋습니다. 또한 파일을 열었으면 마지막에 꼭 닫아주어야 하며, 에러가 발생할 수 있는 부분은 예외 처리를 해주어야 합니다. 더 자세한 내용은 서적이나 인터넷을 참고하여 공부할 수 있습니다.

## 관련 자료

- [하스켈 공식 문서](https://www.haskell.org/documentation/)
- [하스켈 책 추천](https://pranshujain.com/haskell-books/)
- [텍스트 파일 관련 함수들](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#g:17)
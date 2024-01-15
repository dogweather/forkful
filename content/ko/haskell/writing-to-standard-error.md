---
title:                "표준 에러에 쓰는 것"
html_title:           "Haskell: 표준 에러에 쓰는 것"
simple_title:         "표준 에러에 쓰는 것"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

표준 오류 스트림에 쓰는 것이 중요한 이유는 프로그래밍 시 에러를 파악하고 수정하기 위해서입니다.

## 어떻게 할까요

```Haskell
import System.IO

main = do
  hPutStrLn stderr "이것은 스트림에 쓰는 에러 메시지입니다."
```

위의 코드를 실행하면 표준 오류 스트림에 메시지가 출력됩니다. 이는 코드를 디버깅할 때 유용하며, 사용자에게 어떤 프로그램 오류가 발생했는지 알려줄 수 있습니다.

## 깊이 파고들기

표준 오류 스트림에 쓰는 것은 프로그램 개발 과정에서 유용한 디버깅 도구입니다. 일반적으로 프로그램의 출력은 표준 출력 스트림에 나타나며, 에러 메시지는 표준 오류 스트림에 나타납니다. 따라서 우리는 맞춤형 로그 파일을 만들지 않고도 콘솔에 프로그램의 실행 상태를 즉시 확인할 수 있습니다.

## 참고 자료

- [Haskell 공식 문서](https://www.haskell.org/documentation/)
- [표준 스트림에 대한 자세한 설명](https://en.wikipedia.org/wiki/Standard_streams)
- [Haskell 표준 라이브러리 문서](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
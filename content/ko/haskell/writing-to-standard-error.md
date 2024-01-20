---
title:                "표준 오류에 쓰는 방법"
html_title:           "Haskell: 표준 오류에 쓰는 방법"
simple_title:         "표준 오류에 쓰는 방법"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
프로그래머들은 프로그램 실행 중 발생하는 오류나 경고 메시지를 처리하기 위해 표준 에러로 쓰기를 사용합니다. 표준 에러는 프로그램 실행 중 발생하는 오류 메시지를 사용자에게 보여줍니다. 이 방법을 사용하는 이유는 사용자에게 오류에 대해 알리고 이를 해결하도록 유도하기 위해서입니다.

## 방법:
```Haskell
main = do
    putStrLn "Hello World!"
    hPutStrLn stderr "This is an error message!"
```

출력:

```
Hello World!
This is an error message!
```

## 깊이 파고들기:
이 방식은 예전에는 컴퓨터 화면에 직접 에러 메시지를 출력했지만, 프로그래밍 언어가 발전함에 따라 오류 메시지를 사용자에게 보여주는 방식이 바뀌었습니다. 더 나은 대안으로는 로깅 라이브러리를 사용하는 것이 있으며, 이는 오류 메시지를 파일에 기록하거나 네트워크를 통해 전송할 수 있도록 해줍니다. 표준 에러를 이용해 오류 메시지를 사용자에게 제공하는 방식은 안전하고 간단하기 때문에 여전히 널리 사용되고 있습니다.

## 참고 자료:
- [Haskell 표준 라이브러리 문서](https://hackage.haskell.org/package/base-4.11.1.0/docs/GHC-IO-Handle.html#v:hPutStrLn)
---
title:                "텍스트 파일 작성하기"
html_title:           "Haskell: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜?

문자 파일을 쓰기는 우리에게 무엇을 할 수 있는지 나열하고 들어보기 없고 오버스펙하지 않게 할 수 있게 합니다. 이것은 또한 우리가 데이터를 저장하고 공유하는데 유용합니다.

## 사용 방법

문자 파일을 쓰는 가장 간단한 방법은 내장된 `writeFile` 함수를 사용하는 것입니다. 이 함수는 `FilePath` (파일의 경로)와 `String` (쓰고자 하는 내용)을 인자로 받습니다. 예를 들어, 다음과 같이 파일에 "Hello World!"를 쓸 수 있습니다.

```Haskell
writeFile "hello.txt" "Hello World!"
```

이렇게 쓴 파일은 현재 작업 디렉토리에 생성됩니다. 만약 다른 디렉토리에 파일을 쓰고 싶다면 파일 경로를 변경해주면 됩니다.

문자 파일을 읽어올 때는 내장된 `readFile` 함수를 사용할 수 있습니다. 이 함수는 `FilePath`를 인자로 받아 해당 파일의 내용을 문자열로 반환합니다. 이를 통해 파일에 쓴 내용을 다시 읽어올 수 있습니다.

## 깊이 파고들기

Haskell은 문자 파일을 쓰는 데에 있어서 유용한 다양한 함수들을 제공합니다. `appendFile` 함수는 파일에 내용을 이어쓸 수 있게 해줍니다. `openFile` 함수를 사용하면 파일을 열고 여러가지 작업을 할 수 있습니다. 또한 `hClose` 함수를 사용하여 파일을 닫아줘야 합니다.

또한 `writeFile` 함수는 `IO ()` 타입을 반환하는데, 이는 모나드로 IO 작업이라는 것을 나타내줍니다. IO 작업은 순차적으로 실행되어야만 하는 작업이기 때문에 순서에 주의해야 합니다. 또한 파일을 쓴 후에는 꼭 파일을 닫아줘야 합니다.

## 연관 링크

[Haskell 입문](https://ko.wikibooks.org/wiki/Haskell/%EA%B0%95%EC%A2%8C), [Haskell 문서](https://www.haskell.org/documentation/), [Haskell 공식 홈페이지](https://www.haskell.org/)
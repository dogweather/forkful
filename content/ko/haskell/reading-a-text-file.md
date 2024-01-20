---
title:                "텍스트 파일 읽기"
html_title:           "Bash: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

텍스트 파일 읽기는 파일로부터 텍스트 데이터를 추출하는 과정입니다. 프로그래머는 이를 사용해 파일 데이터를 읽고 분석할 수 있습니다.

## 실행 방법:

텍스트 파일을 읽는 기본 예제는 다음과 같습니다:

```Haskell
import System.IO

main = do
    handle <- openFile "test.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
```

위 프로그램을 실행하면 'test.txt' 파일의 내용이 출력됩니다.

## 더 깊이 들어가기:

평문 파일 읽기는 첫 컴퓨터가 만들어진 이후부터 존재했습니다. Haskell에서는 본래 `System.IO` 라이브러리를 통해 파일 읽기를 제공하며, 시간이 지나면서 패턴이 자주 반복되는 것을 깨닫고 라이브러리를 개선하는 방식으로 더욱 단순화되었습니다.

`openFile` 및 `hGetContents` 같은 함수 외에도, Haskell은 파일을 읽기 위한 여러 가지 대안 방법을 제공합니다. 가장 간단한 방법은 `readFile` 함수를 사용하는 것입니다. 

```Haskell
main = do
    contents <- readFile "test.txt"
    putStr contents
```

`readFile`은 파일 이름을 가져와 내용을 문자열로 반환합니다. 이 방식은 소스를 단순화하지만, `hGetContents` 같은 함수에 비해 더 적은 제어를 제공합니다.

또한, `openFile` 및 `hGetContents`를 사용하여 텍스트 파일을 읽을 때, I/O 에러 처리는 크게 중요합니다. 파일이 존재하지 않거나 읽기 권한이 없을 경우 등에 대비해 예외 처리를 해야 합니다. Haskell은 이를 위한 방법을 여러 가지 제공합니다.

## 추가 참조:

3. [Learn You a Haskell for Great Good: Input and Output](http://learnyouahaskell.com/input-and-output)
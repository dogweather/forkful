---
title:                "텍스트 파일 읽기"
html_title:           "Haskell: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 것은 프로그래밍에서 중요한 기능 중 하나입니다. 텍스트 파일을 읽을 수 있다면 다양한 데이터에 접근하고 처리할 수 있어 프로그램의 유연성을 높일 수 있습니다.

## 방법

파일을 읽는 다양한 방법 중에서도 Haskell에서는 `System.IO` 라이브러리를 사용하는 것이 가장 간단하고 효율적입니다. 다음은 텍스트 파일을 읽어서 각 라인을 출력하는 예제 코드입니다.
```Haskell
import System.IO

main = do
    handle <- openFile "textfile.txt" ReadMode   -- 파일을 읽기 모드로 열기
    contents <- hGetContents handle               -- 파일 내용을 읽어서 변수에 저장
    putStr contents                              -- 파일 내용 출력
    hClose handle                                -- 파일 닫기
```

위 코드를 실행하면 `textfile.txt`에 있는 내용이 콘솔에 출력됩니다.

## 깊이 파고들기

Haskell에서 파일을 읽는 방법은 크게 두 가지로 나뉩니다. 첫 번째는 위에서 보인 `openFile`과 `hGetContents`를 사용하는 방법이고, 두 번째는 `withFile`을 사용하는 방법입니다. `withFile`을 사용하면 파일을 자동으로 닫아줄 수 있어서 더 안전하고 편리합니다. 또한 `System.IO`에는 `readFile`이라는 함수도 있어서 파일 내용을 한 번에 읽어서 문자열로 반환할 수 있습니다.

## 참고

[Real World Haskell](http://book.realworldhaskell.org/read/io.html)  -- Haskell에서 파일 다루는 방법에 대한 상세한 설명  
[Hackage - System.IO](https://hackage.haskell.org/package/base-4.14.0.0/docs/System-IO.html)  -- `System.IO` 라이브러리의 공식 문서  
[W3Schools - Haskell Files](https://www.w3schools.com/haskell/haskell_files.asp) -- `readFile`을 사용하는 방법에 대한 예제 코드
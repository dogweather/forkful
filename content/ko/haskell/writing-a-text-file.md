---
title:                "텍스트 파일 작성하기"
date:                  2024-01-19
html_title:           "Arduino: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가 & 왜 인가?)
텍스트 파일 쓰기는 데이터를 텍스트 형태로 파일에 저장하는 것입니다. 프로그래머들은 데이터 보존, 로깅, 설정 정보 저장 등을 위해 이 방법을 사용합니다.

## How to: (어떻게 하나)
```Haskell
import System.IO

main :: IO ()
main = do
    let fileName = "example.txt"
    let content = "안녕, Haskell!"
    writeFile fileName content
    putStrLn $ content ++ " -> " ++ fileName
```

실행 결과:
```
안녕, Haskell! -> example.txt
```

## Deep Dive (깊이 탐구)
- **역사적 배경**: Haskell은 1990년대 초에 개발되었으며 파일 I/O 라이브러리는 외부 자원과 상호작용하는 동안 순수성을 유지하는 기능적 접근법을 제공합니다.
- **대안들**: `writeFile` 함수 외에, 더 복잡한 작업을 위해 `openFile`, `hPutStr`, `hClose` 같은 낮은 레벨의 API를 사용할 수 있습니다.
- **구현 세부 사항**: `writeFile`은 내부적으로 파일을 여는 작업, 데이터 쓰기, 파일 닫기 작업을 자동으로 처리합니다. 오류 발생 시 예외가 발생할 수 있으므로 예외 처리를 고려해야 합니다.

## See Also (관련 자료)
- [Haskell Documentation: Input and Output](https://www.haskell.org/tutorial/io.html)
- [Haskell `System.IO` module](http://hackage.haskell.org/package/base-4.16.0.0/docs/System-IO.html)

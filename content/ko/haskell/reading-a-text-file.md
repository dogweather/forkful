---
title:                "Haskell: 텍스트 파일 읽기"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜
텍스트 파일을 읽는 것의 중요성에 대해 알려주기 위해서입니다. 텍스트 파일을 읽는 것은 프로그래밍에서 필수적인 요소이며, 데이터를 분석하고 처리하는 데 필수적입니다.

## 어떻게
텍스트 파일을 읽는 것은 Haskell에서 매우 간단합니다. 먼저 ```import System.IO```를 입력해 주세요. 그 다음에는 텍스트 파일의 경로를 입력하여 파일 핸들을 열어주면 됩니다.
```
import System.IO
main = do
    handle <- openFile "text.txt" ReadMode
```

이제 ```handle``` 변수를 사용하여 파일 내용을 읽을 수 있습니다. 예를 들어, 파일의 첫 번째 라인을 읽어보겠습니다.
```
line <- hGetLine handle
putStrLn line
```

위의 코드를 실행하면 텍스트 파일의 첫 번째 라인이 콘솔에 출력될 것입니다. 이와 같은 방식으로 다른 라인도 읽을 수 있습니다.

만약 파일을 모두 읽고 싶다면, ```handle``` 변수를 사용하여 파일을 끝까지 읽을 수 있습니다.
```
contents <- hGetContents handle
putStrLn contents
```

위의 코드를 실행하면 텍스트 파일의 내용이 전부 콘솔에 출력될 것입니다.

## 심층 분석
실제로 텍스트 파일을 읽을 때는 파일을 열고 닫는 과정을 꼭 수행해야 합니다. 위의 예제에서는 파일을 열고 난 후에는 파일을 닫지 않았기 때문에 나중에 문제가 발생할 수 있습니다. 따라서 파일을 열 때는 항상 적절한 방법으로 파일을 닫아주는 것이 중요합니다.

또한, 위의 예제에서는 텍스트 파일을 한 줄씩 읽었지만, 실제로는 더 복잡한 파일을 다루어야 할 때도 있습니다. 예를 들어, CSV 파일이나 JSON 파일 등의 형식을 다루는 경우도 있습니다. 이럴 때는 파일 내용을 적절하게 파싱하여 원하는 데이터를 추출하는 것이 중요합니다.

## 관련 자료
- [Haskell: Reading and Writing Files](https://www.tutorialspoint.com/haskell/haskell_input_output.htm)
- [Real World Haskell - File I/O and Exceptions](http://book.realworldhaskell.org/read/io.html)
- [Haskell Language Specification - Input and Output](https://www.haskell.org/onlinereport/io.html#ix2994)
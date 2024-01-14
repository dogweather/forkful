---
title:    "Haskell: 텍스트 파일 읽기"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 것에 대해 관심이 있는 이유는 프로그래밍을 할 때 자주 사용되기 때문입니다. 우리는 텍스트 파일을 읽는 것을 통해 데이터를 가져오고, 처리하고, 결과를 출력할 수 있습니다.

## 사용 방법

이제 Haskell을 사용하여 텍스트 파일을 읽고, 데이터를 처리하고, 결과를 출력해보겠습니다. 먼저 파일을 열고, 파일 내용을 모두 읽은 뒤에는 닫아주어야 합니다. 이를 위해 `withFile` 함수를 사용합니다. 

그 다음에는 `hGetLine` 함수를 사용하여 파일 내의 한 줄씩 읽어올 수 있습니다. 이를 응용하여 파일 내의 모든 데이터를 불러오는 코드를 작성해보겠습니다.

\```Haskell
import System.IO

main :: IO ()
main = do
  withFile "sample.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    putStrLn contents
\```

위 코드를 실행하면 `sample.txt` 파일 내의 모든 내용이 출력됩니다.

## 깊게 파고들기

실제로 텍스트 파일을 읽으면서 사용할 수 있는 다양한 함수들이 있습니다. 예를 들어, 파일 내의 특정 패턴을 검색하는 `hSearch` 함수나 파일 내의 특정 위치로 이동하는 `hSeek` 함수 등이 있습니다. 자세한 정보는 공식 문서를 참고하기를 권장합니다.

## 봐도 좋아요

- [Haskell 공식 문서](https://www.haskell.org/documentation/) 
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters)
- [Real World Haskell](http://book.realworldhaskell.org/read/)
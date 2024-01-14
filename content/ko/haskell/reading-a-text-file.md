---
title:    "Haskell: 텍스트 파일 읽기"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# 왜

텍스트 파일을 읽는 것이 왜 중요한지를 이해하는 것은 프로그래밍에서 매우 중요합니다. 텍스트 파일은 데이터를 저장하고 공유하는 데 매우 유용하며 읽기 쉽고 수정하기도 쉽습니다. 하스켈을 사용하여 텍스트 파일을 읽는 방법에 대해 알아봅시다.

## 어떻게

우리의 첫 번째 예제는 매우 간단합니다. 아래 코드는 "test.txt" 파일의 내용을 읽고, 콘솔에 한 줄씩 출력하는 코드입니다.

```Haskell
main = do
  contents <- readFile "test.txt"
  let linesOfFile = lines contents
  mapM_ putStrLn linesOfFile
```

위 코드를 실행하여 "test.txt" 파일의 모든 내용이 콘솔에 출력되는 것을 볼 수 있습니다.

## 깊게 파헤치기

하지만 이것만으로는 충분하지 않습니다. 텍스트 파일의 내용을 즉시 콘솔에 출력하는 것보다 더 유용한 방법이 있습니다. 이는 두 단계로 나눠서 처리할 수 있습니다. 첫째, 파일의 내용을 읽어서 리스트로 저장합니다. 둘째, 리스트를 순회하면서 각 줄을 처리합니다. 아래 코드는 파일의 내용을 읽어서 앞뒤 공백을 제거하고 길이가 10 이상인 줄을 콘솔에 출력하는 코드입니다.

```Haskell
main = do
  contents <- readFile "test.txt"
  let linesOfFile = map (trim . strip) (lines contents)
  let longLines = filter (\line -> length line > 10) linesOfFile
  mapM_ putStrLn longLines
  where
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
    strip = filter (/= '\r')
```

위 코드를 실행하면 파일의 내용 중에서 길이가 10 이상인 줄만 콘솔에 출력되는 것을 볼 수 있습니다.

# 관련 사이트

- [하스켈 공식 홈페이지](https://www.haskell.org/)
- [하스켈 문서](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html)
- [하스켈 커뮤니티 포럼](https://discourse.haskell.org/)
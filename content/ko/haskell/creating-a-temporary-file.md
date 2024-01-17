---
title:                "임시 파일 생성하기"
html_title:           "Haskell: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

프로그래머들은 일시적인 파일을 만들고 사용하는 이유는 여러 가지가 있습니다. 일시적인 파일은 메모리를 더 사용할 수 없는 경우에 유용합니다. 또는 일시적인 파일을 사용하면 프로그램이 계산을 계속 할 수 있기 때문에 재시작할 필요가 없습니다.

## 사용 방법:

이 기능을 사용하려면 아래와 같이 코드를 작성하면 됩니다:

```Haskell
import System.IO

main = do
  tempFile <- openTempFile "." "temp"
  hClose $ snd tempFile

  putStrLn $ "새로운 일시적인 파일이 " ++ (fst tempFile) ++ "에 생성되었습니다."
  putStrLn $ "일시적인 파일을 사용하여 기능을 수행합니다."
  writeFile (fst tempFile) "Hello, World!"

  putStrLn $ "기능이 끝나면 파일을 삭제합니다."
  removeFile (fst tempFile)

  putStrLn "완료!"
```

이 예제에서는 시스템 모듈의 `openTempFile` 함수를 사용하여 일시적인 파일을 만들고 `hClose` 함수를 사용하여 파일을 닫습니다. 그리고 파일의 경로를 `fst` 함수를 사용하여 가져와서 기능을 수행합니다. 마지막으로 `removeFile`을 사용하여 파일을 삭제합니다.

예상 출력:

```
새로운 일시적인 파일이 /Users/temp에 생성되었습니다.
일시적인 파일을 사용하여 기능을 수행합니다.
기능이 끝나면 파일을 삭제합니다.
완료!
```

## 깊은 듯:

(1) 일시적인 파일은 프로그래머들에게 많은 이점을 제공합니다. 예를 들어, 운영체제의 제한으로 인해 메모리를 더 이상 사용할 수 없을 때 유용합니다. 또한 일시적인 파일은 재시작 없이 계산을 계속할 수 있도록 해주며, 매번 많은 작업을 수행할 필요가 없기 때문에 더욱 효율적입니다.

(2) 일시적인 파일을 만드는 다른 방법으로는 `withSystemTempFile` 함수와 `withTempFile` 함수가 있습니다. 이들 함수는 자동으로 파일을 생성하고 삭제해주기 때문에 더욱 편리합니다.

(3) 일시적인 파일은 일시적으로만 사용되기 때문에 디스크 공간을 낭비하지 않는 것이 중요합니다. 따라서 일시적인 파일은 프로그래밍 언어 레벨에서 최적화되어야 합니다.

## 관련 자료:

- [Haskell 언어 공식 웹사이트](https://www.haskell.org/)
- [중급 하스켈 프로그래밍 강좌](https://wiki.haskell.org/A_tutorial_for_Programming_in_Haskell)
- [시스템 모듈 문서](http://hackage.haskell.org/package/base-4.8.2.0/docs/System.html)
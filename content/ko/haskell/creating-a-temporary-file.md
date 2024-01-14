---
title:    "Haskell: 임시 파일 만들기"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## 왜 

임시 파일을 만드는 이유는 여러 가지가 있습니다. 예를 들어, 프로그램 실행 중에 데이터를 일시적으로 저장하거나, 다른 프로그램과의 통신을 위해 임시 파일을 사용할 수 있습니다. 또는 파일 시스템에 영구적으로 저장하기 전에 임시 파일을 사용하여 데이터의 유효성을 테스트할 수도 있습니다. 

## 만드는 방법 

임시 파일을 만드는 가장 간단한 방법은 `System.IO` 모듈의 `withSystemTempFile` 함수를 사용하는 것입니다. `withSystemTempFile` 함수는 임시 파일의 이름과 핸들러를 매개변수로 받아 해당 임시 파일을 사용하는 작업을 수행합니다. 예제 코드는 다음과 같습니다.

```Haskell
import System.IO (withSystemTempFile)

main = withSystemTempFile "temp.txt" $ \tempHandle tempName -> do
    putStrLn ("작업할 임시 파일 이름: " ++ tempName)
    hPutStrLn tempHandle "임시 파일에 작성할 내용입니다."
```

위 코드를 실행하면 지정한 임시 파일 `temp.txt`가 생성되고, 해당 파일에 문자열 "임시 파일에 작성할 내용입니다."가 저장됩니다. 파일 처리 작업이 완료되면 `withSystemTempFile` 함수는 자동으로 임시 파일을 삭제하므로 별도의 삭제 코드를 작성할 필요가 없습니다.

## 깊게 파헤치기 

`withSystemTempFile` 함수는 자동으로 임시 파일을 생성하고 삭제하기 때문에 편리하지만, 임시 파일의 생성 방식에 대해 더 자세히 알아보고 싶을 수도 있습니다. `withSystemTempFile`의 정의를 살펴보면 다음과 같은 형식으로 되어 있습니다.

```
withSystemTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
```

첫 번째 매개변수는 임시 파일의 이름으로, 두 번째 매개변수는 임시 파일의 이름과 핸들을 매개변수로 받는 함수입니다. 이 함수를 통해 실제로 임시 파일을 사용하고자 하는 작업을 수행할 수 있습니다. 임시 파일을 생성한 뒤, 해당 파일을 사용하는 작업을 수행하고, 작업이 완료되면 임시 파일을 삭제하는 것입니다.

보다 자세한 내용은 [Haskell 공식 문서](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html#v:withSystemTempFile)를 참고하세요.

## 관련 링크 

- [Haskell의 임시 파일 처리](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-the-haskell-acks/exception-safe-file-management)
- [IO 모나드에 대한 심도있는 이해](https://www.youtube.com/watch?v=pbnzQTMvRlU)
- [Haskell로 파일 처리하기](https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/11_IO)
- [Haskell 공식 문서](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
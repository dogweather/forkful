---
title:                "Haskell: 임시 파일 만들기"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 왜 임시 파일을 만드는가

임시 파일은 프로그래밍에서 매우 중요한 개념 중 하나입니다. 임시 파일은 우리가 작성한 프로그램이 데이터를 임시로 저장하는 데 사용됩니다. 이는 프로그램이 실행되는 동안 일시적으로 필요한 파일을 생성하고 사용한 후에는 자동으로 삭제하여 우리의 컴퓨터를 깔끔하게 유지할 수 있게 해줍니다.

## 만드는 방법

임시 파일을 만드는 방법은 다양한 언어에서 다소 차이가 있습니다. 하지만 우리는 지금 Haskell을 다룰 것이기 때문에 Haskell을 이용한 임시 파일 생성 방법을 알아보겠습니다.

```Haskell
import System.IO
import System.Directory
import System.IO.Temp

main = do
    -- withSystemTempFile 함수를 이용해 임시 파일을 생성합니다.
    withSystemTempFile "test.txt" $ \tempFilePath tempHandle -> do
        -- 임시 파일에 데이터를 씁니다.
        hPutStrLn tempHandle "This is a test file."
        -- 임시 파일의 경로를 출력합니다.
        putStrLn tempFilePath
```

위의 코드에서는 `withSystemTempFile` 함수를 사용하여 임시 파일을 생성하고 거기에 데이터를 쓴 후 임시 파일의 경로를 출력하도록 설정했습니다. `withSystemTempFile` 함수는 임시 파일의 경로와 파일 핸들을 반환하며, 파일 핸들은 생성 후 자동으로 닫히도록 설정되어 있습니다.

실행 결과는 다음과 같습니다:

```
C:\Users\username\AppData\Local\Temp\test91900.txt
```

위의 예제는 간단하지만, 실제 프로그램에서는 이러한 임시 파일을 사용하여 데이터를 저장하고 다루는 작업을 많이 수행하게 됩니다.

## 더 들어가기

Haskell에서 임시 파일을 만들어 사용하는 방법에 대해 더 알아보도록 하겠습니다. 위의 예제에서는 `withSystemTempFile` 함수를 사용하였지만, `createTempFile` 함수를 사용하여도 임시 파일을 생성할 수 있습니다. 다만, `createTempFile` 함수를 사용할 경우 파일 핸들의 생성 및 닫힘에 대한 처리를 직접 해주어야 합니다.

또한, 위의 예제에서는 임시 파일의 경로를 출력하기만 하지만, `withSystemTempFile` 함수에서 생성한 임시 파일에는 다양한 작업을 할 수 있습니다. 예를 들어, 파일 내용을 읽거나 파일 핸들을 이용해 데이터를 쓸 수 있습니다. 이러한 작업은 실제 프로그램에서 매우 유용하게 사용될 수 있습니다.

# 더 알아보기

이번 포스트에서는 Haskell을 이용해 임시 파일을 생성하는 방법에 대해 알아보았습니다. 임시 파일은 프로그래밍에서 매우 유용하게 사용되므로, 학습하시는 분들은 자유롭게 다루어보시기 바랍니다.

## 관련 링크

- [Haskell 임시 파일 생성하기](https://hackage.haskell.org/package/temporary-1.3/docs/System-IO-Temp.html)
- [Haskell 파일 핸들 다루기](https://hsdev.kr/posts/2020-07-14-haskell-handle-amp-file-write.html)
- [Haskell 파일 입출력](https://hsdev.kr/posts/2020-07-12-haskell-a-simple-io.html)
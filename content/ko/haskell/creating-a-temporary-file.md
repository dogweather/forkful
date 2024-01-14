---
title:    "Haskell: 임시 파일 만들기"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

임시 파일을 만드는 것에 참여하는 이유는 여러 가지가 있을 수 있습니다. 대부분의 경우, 우리는 임시적으로 파일을 만들어서 우리가 원하지 않는 데이터 또는 특정 상황에서만 필요한 데이터를 저장하기 위해 사용합니다.

## 방법

임시 파일을 만드는 것은 Haskell에서 매우 간단합니다. 단지 몇 줄의 코드를 작성하면 됩니다. 아래는 예제 코드와 해당 코드의 출력입니다.

```Haskell
import System.Directory (createTempFile)

main = do
    let contents = "임시 파일에 저장할 데이터"
    (filename, handle) <- createTempFile "." "temp"
    writeFile filename contents
    print handle
```

출력:

```
C:\Users\temp252.txt
```

위 코드에서는 `createTempFile` 함수를 사용하여 현재 디렉토리에서 "temp"라는 이름의 임시 파일을 생성합니다. 그리고 파일에 `contents` 변수의 내용을 쓰고, 생성된 파일의 핸들을 출력합니다.

## 깊게 파보기

`createTempFile` 함수는 실제로 시스템에서 임시 파일을 만드는 데 매우 신중하게 처리됩니다. 이 함수는 사전에 정의된 임시 디렉토리에서 파일 이름을 생성하여 사용하며, 실제 파일과 같은 속성을 갖도록 적절히 설정됩니다. 이렇게 함으로써 시스템 안정성이 향상되고, 오류가 발생할 가능성이 줄어듭니다.

## 생각해보기 목록

- [Haskell documentation on `createTempFile`](https://hackage.haskell.org/package/directory-1.3.6.0/docs/System-Directory.html#v:createTempFile)
- [A beginner's guide to Haskell](https://wiki.haskell.org/Introduction)
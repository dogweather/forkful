---
title:                "명령줄 인수 읽기"
html_title:           "Arduino: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
명령 줄 인자 읽기는 프로그램에 인자를 전달하는 방법입니다. 이를 통해 프로그래머는 사용자가 명령 줄에서 직접 파라미터를 입력하여 프로그램의 동작을 제어할 수 있습니다.

## 어떻게 해야 할까?
Haskell에서 명령 줄 인자를 읽는 것은 간단합니다. `System.Environment` 라이브러리의 `getArgs` 함수를 사용하면 됩니다.

```Haskell
import System.Environment 

main = do
    args <- getArgs  
    putStrLn ("Hello, " ++ args !! 0)
```
이 코드를 실행하면, 입력한 첫번째 인자를 출력합니다.

```bash
$ ghc hello.hs
$ ./hello World
Hello, World
```

## 깊게 보기
명령 줄 인자를 읽는 방법은 Unix 시스템이 처음 개발되었을 때부터 사용되고 있습니다. 이를 통해 프로그램에 필요한 데이터를 쉽게 전달할 수 있습니다. Haskell에서는 위에서 언급한 `getArgs` 외에도 `getProgName` 함수로 프로그램 이름을 얻을 수 있습니다.

```Haskell
import System.Environment 

main = do
    progName <- getProgName
    putStrLn ("Program name: " ++ progName)

```
이 코드를 실행하면, 실행 파일의 이름을 출력합니다.

```bash
$ ghc progName.hs
$ ./progName
Program name: progName
```

대안으로는 `getEnv` 함수를 사용하여 환경 변수를 직접 읽는 방법이 있습니다.

## 참고 자료

- `System.Environment` 문서: [http://hackage.haskell.org/package/base-4.14.1.0/docs/System-Environment.html](http://hackage.haskell.org/package/base-4.14.1.0/docs/System-Environment.html)
- 더 깊은 이해를 위한 Haskell 튜토리얼: [https://www.haskell.org/tutorial/](https://www.haskell.org/tutorial/)
---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Bash: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

디렉토리 존재 검사는 파일 시스템에서 특정 디렉토리의 존재 여부를 체크하는 것을 의미합니다. 프로그래머들은 파일이나 데이터를 제대로 처리하기 위해 이시중요하다는 것을 알고 있습니다.

## 어떻게 하는가:

Haskell에서 디렉토리 존재를 검사하기 위해 `System.Directory`라는 라이브러리를 사용합니다. 먼저, 필요한 모듈을 임포트해야합니다.

```Haskell
import System.Directory (doesDirectoryExist)
```

다음은 간단한 예제입니다.

```Haskell
main :: IO ()
main = do
    let dirPath = "/path/to/dir"
    exists <- doesDirectoryExist dirPath
    putStrLn $ "Does directory " ++ dirPath ++ " exist? " ++ show exists
```

이 코드를 실행하면 다음과 같은 출력을 보게 됩니다:

```Haskell
Does directory /path/to/dir exist? True
```

## 심화 학습:

Haskell의 `System.Directory` 모듈에서 제공하는 `doesDirectoryExist` 함수는 5.2.1.1 버전(2001년에 릴리즈된 GHC 5.02.2부터 사용 가능)부터 사용 가능합니다. 이 함수를 사용하는 대신, 사용자가 직접 POSIX 시스템 콜을 이용해서 직접 구현할 수도 있지만, 사실상 모든 경우에 `doesDirectoryExist` 함수를 사용하는 것이 바람직합니다.

## 참고자료:

1. [Haskell Documentation : System.Directory](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html)
2. [Learn Haskell for Great Good!](http://learnyouahaskell.com)
3. [Haskell Tutorial on Reading, Writing and Appending Files](https://www.tutorialspoint.com/haskell/haskell_file_io.htm)
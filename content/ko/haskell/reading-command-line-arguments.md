---
title:                "명령줄 인수 읽기"
html_title:           "Haskell: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 무엇이고 왜 필요한가?
명령 줄 인수를 읽는 것은 데이터를 입력으로 받아들이기 위해 프로그램에서 사용하는 일반적인 기술입니다. 프로그래머는 이를 통해 사용자로부터 다양한 유형의 입력을 받고, 이를 프로그램 내에서 처리하고 응답하는 데 사용할 수 있습니다.

# 사용 방법:
Haskell에서 명령 줄 인수를 읽는 방법은 간단합니다. 먼저 `System.Environment` 모듈을 가져와야 합니다. 그 다음 `getArgs` 함수를 사용하여 입력된 인수들을 가져올 수 있습니다. 예를 들어, 다음과 같이 코드를 작성할 수 있습니다:
```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn $ "입력된 인수: " ++ show args
```
이 코드를 실행하면 `getArgs` 함수로부터 입력된 인수들을 가져와서 콘솔에 출력하게 됩니다.

### 출력 예:
```
입력된 인수: ["Hello", "World", "123"]
```

# 깊이 들여다보기:
명령 줄 인수를 읽는 기술은 운영 체제의 커맨드 라인 인터페이스와 밀접한 관련이 있습니다. 초기 컴퓨터에서는 명령 줄 인수를 읽는 것이 프로그램의 꼭대기에 위치한 `argv` 라는 변수로 이루어졌지만, 이후 이를 더 쉽게 사용할 수 있는 함수들이 개발되었습니다. 이러한 함수들은 Haskell에서도 여전히 사용되고 있습니다.

다른 언어에서는 명령 줄 인수를 읽는 다른 방법들도 존재합니다. 예를 들어, C언어의 경우 `getopt` 함수를 사용할 수 있습니다. 그러나 Haskell에서는 위에서 소개한 `System.Environment` 모듈을 통해 간편하게 명령 줄 인수를 읽을 수 있습니다.

# 관련 자료:
- [Haskell Command Line Arguments](https://wiki.haskell.org/Command_line_option_parsing)
- [System.Environment Haskell Documentation](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Environment.html)
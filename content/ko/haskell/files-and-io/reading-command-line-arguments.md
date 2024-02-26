---
date: 2024-01-20 17:56:19.596598-07:00
description: "\uBA85\uB839\uC904 \uC778\uC218 \uC77D\uAE30(Command line arguments\
  \ reading)\uB294 \uC0AC\uC6A9\uC790\uAC00 \uD504\uB85C\uADF8\uB7A8\uC744 \uC2DC\uC791\
  \uD560 \uB54C \uBA85\uB839\uC904\uC744 \uD1B5\uD574 \uD504\uB85C\uADF8\uB7A8\uC5D0\
  \ \uB370\uC774\uD130\uB97C \uC804\uB2EC\uD558\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC0AC\uC6A9\uC790\uC758 \uC785\uB825\
  \uC744 \uBC1B\uC544 \uCEE4\uC2A4\uD130\uB9C8\uC774\uC988\uB41C \uD589\uB3D9\uC744\
  \ \uD558\uAE30 \uC704\uD574 \uC774\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-02-25T18:49:52.312686-07:00'
model: gpt-4-1106-preview
summary: "\uBA85\uB839\uC904 \uC778\uC218 \uC77D\uAE30(Command line arguments reading)\uB294\
  \ \uC0AC\uC6A9\uC790\uAC00 \uD504\uB85C\uADF8\uB7A8\uC744 \uC2DC\uC791\uD560 \uB54C\
  \ \uBA85\uB839\uC904\uC744 \uD1B5\uD574 \uD504\uB85C\uADF8\uB7A8\uC5D0 \uB370\uC774\
  \uD130\uB97C \uC804\uB2EC\uD558\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uC0AC\uC6A9\uC790\uC758 \uC785\uB825\uC744 \uBC1B\
  \uC544 \uCEE4\uC2A4\uD130\uB9C8\uC774\uC988\uB41C \uD589\uB3D9\uC744 \uD558\uAE30\
  \ \uC704\uD574 \uC774\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBA85\uB839\uC904 \uC778\uC218 \uC77D\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
명령줄 인수 읽기(Command line arguments reading)는 사용자가 프로그램을 시작할 때 명령줄을 통해 프로그램에 데이터를 전달하는 방법입니다. 프로그래머들은 사용자의 입력을 받아 커스터마이즈된 행동을 하기 위해 이를 사용합니다.

## How to: (방법)
Haskell에서 명령줄 인수를 읽는 것은 `System.Environment` 모듈을 사용합니다. 간단한 예제를 보겠습니다.

```Haskell
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Hello, " ++ head args ++ "!")
```

실행 결과는 다음과 같습니다.
```
$ runghc greetings.hs Sarah
Hello, Sarah!
```
이 예에서, `getArgs` 함수는 모든 명령줄 인수를 문자열의 리스트로 반환합니다. `head` 함수로 첫 번째 인수를 가져옵니다.

## Deep Dive (심화 학습)
- **역사적 맥락**: Haskell의 `System.Environment` 모듈은 프로그램에 전달된 인수를 다룰 수 있는 기능을 부여합니다. 표준 모듈이기 때문에 설치 없이 사용 가능합니다.
- **대안들**: 인수 파싱 라이브러리들, 예를 들면 `optparse-applicative`, `cmdargs`, `getopt` 같은 것들이 더 복잡한 인수와 옵션을 처리할 때 도움을 줍니다.
- **구현 세부 사항**: `getArgs`는 `IO [String]`을 반환하므로, `IO` Monad 내부에서 사용해야 합니다. 패턴 매칭이나 리스트 함수를 사용해 인수를 조작합니다.

## See Also (관련 자료)
- [Haskell System.Environment Documentation](https://hackage.haskell.org/package/base/docs/System-Environment.html)
- [Haskell "optparse-applicative" Library](https://hackage.haskell.org/package/optparse-applicative)
- [Haskell "cmdargs" Library](https://hackage.haskell.org/package/cmdargs)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)

명령줄 인수와 관련된 정보나 라이브러리는 Haskell 패키지 문서에서 더 찾아볼 수 있습니다. "Learn You a Haskell"은 Haskell 배우기에 유용한 자원입니다.

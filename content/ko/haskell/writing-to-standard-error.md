---
title:                "표준 오류로 쓰기"
date:                  2024-01-19
html_title:           "Bash: 표준 오류로 쓰기"
simple_title:         "표준 오류로 쓰기"

category:             "Haskell"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

표준 오류는 오류 메시지와 로그를 출력하는 스트림이다. 프로그래머들은 데이터 출력과 오류 메시지를 분리하기 위해 표준 오류를 쓴다.

## How to: (방법)

표준 오류로 쓰려면 `System.IO` 모듈을 사용해야 합니다.

```Haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "이 메시지는 표준 오류에 나타납니다."
```

출력 예시:
```
이 메시지는 표준 오류에 나타납니다.
```

## Deep Dive (심층 분석)

- 역사적 맥락: UNIX 시스템에서 시작, 표준 출력(stdout)과 분리되어 에러 처리를 용이하게 함.
- 대안: 일부 프로그래머는 로깅 라이브러리를 사용하거나 파일로 직접 기록할 수 있음.
- 구현 세부사항: `System.IO` 모듈의 `stderr`는 `Handle` 타입으로, `hPutStr`나 `hPutStrLn`과 같은 함수로 쓸 수 있음.

## See Also (참고 자료)

- Haskell의 `System.IO` 문서: [Hackage System.IO](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- 표준 스트림에 관한 위키백과 문서: [Wikipedia Standard Streams](https://en.wikipedia.org/wiki/Standard_streams)
- 표준 오류 출력에 대한 더 깊은 토론: [Stack Overflow Discussion](https://stackoverflow.com/questions/tagged/standard-error+haskell)

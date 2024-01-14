---
title:    "Haskell: 표준 오류에 쓰는 방법"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# 왜 작성을 표준 오류에 대해 쓸까요?
개발자들은 종종 표준 출력 대신 표준 오류에 메시지를 보내는 것으로 결정합니다. 이는 프로그램의 디버깅과 에러 처리를 더 쉽게 만들어줍니다. 또한 터미널 환경에서 직접 에러를 확인할 수 있기 때문에, 사용자와 상호 작용하는 프로그램에서 유용합니다.

## 어떻게 하나요?
Haskell에서 표준 오류에 메시지를 보내기 위해서는 `System.IO` 모듈의 `hPutStrLn` 함수를 사용해야 합니다. 아래는 간단한 예시입니다:

```Haskell
import System.IO

main = do
    hPutStrLn stderr "이것은 에러 메시지입니다."
```
위 코드를 실행하면, 화면에는 아무것도 출력되지 않고 `stderr`에만 메시지가 출력됩니다.

## 더 깊게 들어가보기
Haskell에서는 `stderr` 대신 `hPutStr` 함수를 사용할 수도 있습니다. 이 함수는 줄바꿈을 추가하지 않고 기존 줄에 이어서 출력합니다. 또한 `System.IO` 모듈에는 `stderr`를 기본 출력으로 설정하는 `hSetBuffering` 함수도 있습니다.

## 더 많은 정보를 원하신다면?
Haskell에서 표준 오류와 관련된 더 많은 정보를 알고 싶다면 아래 링크들을 참고하시기 바랍니다.

## 또 다른 자료들
- [Haskell 표준 라이브러리 문서](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html#v:hPutStrLn)
- [Haskell Wiki - 표준 입출력](https://wiki.haskell.org/FileIO)
- [Learn You a Haskell for Great Good! - 입출력부터 만들기까지](http://learnyouahaskell.com/input-and-output)
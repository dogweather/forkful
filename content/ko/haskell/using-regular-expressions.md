---
title:    "Haskell: 정규식 사용하기"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜

정규 표현식은 Haskell 프로그래밍에서 매우 유용합니다. 이를 사용하면 텍스트에서 패턴을 찾거나 조작할 수 있습니다. 위대한 언어를 만드는 데 한 몫씩 하기 위해서는 정규 표현식을 배워야 합니다.

## 사용 방법

정규 표현식을 사용하는 가장 간단한 방법은 "ghc"나 "ghci" 프로그램을 사용하는 것입니다. 이 프로그램은 컴파일러 및 인터랙티브 환경입니다. 우선 "ghci"를 열고 다음을 입력해 보세요.

```Haskell
import Text.Regex.TDFA
```
그런 다음 컴파일러에서는 다음을 통해 패턴을 매칭할 수 있습니다.

```Haskell
"Hello World!" =~ "Hello"
```
출력은 "True"가 될 것입니다. 패턴을 찾지 못하면 "False"가 됩니다.

## 깊이 알아보기

정규 표현식에서 사용 가능한 다른 기능도 있습니다. 예를 들어 패턴에서 값을 추출할 수 있습니다. 이는 중첩된 괄호를 사용하여 가능합니다. 올바른 패턴을 사용하면 값을 선택적으로 포함시킬 수 있습니다.

자세한 내용은 "Text.Regex.TDFA" 모듈 문서를 참조하세요. 정규 표현식을 사용하는 더 많은 예제도 찾을 수 있습니다.

## 참고 자료

- [Haskell 정규 표현식 문서](https://www.haskell.org/haskellwiki/Regular_expressions)
- [Haskell 정규 표현식 튜토리얼](https://web.archive.org/web/20121113020535/http://www.dreamsongs.com/Files/RegExp.pdf)
- [Haskell 정규 표현식 사용 예제](https://wiki.haskell.org/Regular_expressions)
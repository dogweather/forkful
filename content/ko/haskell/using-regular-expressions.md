---
title:    "Haskell: 정규 표현식 사용하기"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## 왜

정규식을 사용하는 이유는 데이터에서 패턴을 찾아내기 위해 필요합니다. 정규식은 텍스트를 다룰 때 유용한 도구이며, Haskell에서 이를 사용하면 데이터 처리 과정이 간단해집니다.

## 어떻게

```Haskell
import Text.Regex.Posix

-- 문자열에 대해 정규식 패턴을 만들고 매치하는 함수
matchRegex :: String -> String -> Bool
matchRegex pattern str = str =~ pattern :: Bool

-- 전화번호 패턴을 만들고 매치하는 예시
phoneNumberPattern = "^(01[016789])-([0-9]{3,4})-([0-9]{4})$"
print $ matchRegex phoneNumberPattern "010-1234-5678"
```

```
True
```

위의 코드 예시에서는 `Text.Regex.Posix` 모듈에서 제공하는 `=~` 함수를 사용하여 문자열이 정규식 패턴과 일치하는지 확인할 수 있습니다. 이를 통해 전화번호와 같은 특정한 패턴을 가진 데이터를 쉽게 찾아낼 수 있습니다.

## 깊이 파고들기

정규식은 매우 강력한 도구이지만 문법이 복잡하다는 단점이 있습니다. Haskell에서는 정규식을 작성할 때 까다로운 작은 따옴표와 슬래시를 사용해야 합니다. 또한 공백 또는 특수문자를 어떻게 처리할지에 대한 고민이 필요합니다. 다양한 패턴과 매칭을 시도해보고 실패할 경우 에러 메세지를 사용하여 디버깅하는 것이 중요합니다.

## 참고

- [Haskell 정규식 사용법](https://www.haskell.org/onlinereport/standard-prelude.html#t:Text.Regex.Posix)
- [Haskell 정규식 문법](https://www.haskell.org/haskellwiki/Regex_Tutorial)
- [정규식 체크 도구](https://regexr.com/)
---
title:                "Haskell: 정규 표현식 사용하기"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜?

정규 표현식을 사용하는 이유는 무엇일까요? 정규 표현식은 문자열 내의 패턴을 검색하고 일치하는 문자열을 찾는 강력한 도구입니다. 이를 통해 데이터를 처리하고 조작하는 데 유용하게 사용할 수 있습니다. 또한 많이 사용되는 언어인 Haskell에서 빠르고 쉽게 정규 표현식을 사용할 수 있습니다.

## 어떻게 사용하나요?

```Haskell
-- String 내에서 "apple"이라는 단어를 찾는 예제 코드
import Text.Regex.PCRE

findApple :: String -> Bool
findApple str = str =~ "apple"
```
위의 코드는 "```" 사이에 적힌 부분을 제외하고 Haskell 코드이므로 따로 복사하여 실행시켜보셔야 합니다. 이 코드는 "^apple" 패턴과 일치하는 문자열을 찾는 함수를 정의한 것입니다. "```" 부분에 적힌 패턴을 조정하여 다른 문자열을 찾을 수도 있습니다.

아래는 위의 함수를 이용하여 "I love apples"라는 문자열이 "apple"을 포함하는지 확인하는 예제 코드입니다.

```Haskell
findApple "I love apples" --출력: True
```

## 심층 분석

정규 표현식을 사용하면 문자열을 분석하고 원하는 패턴을 추출할 수 있습니다. 이를 통해 텍스트 파일에서 특정 단어를 찾거나, 이메일 주소를 추출하거나, 특정 문구를 삭제할 수 있습니다. 정규 표현식은 문자열의 특정 부분을 찾는 데 유용한 메타 문자를 포함합니다. 아래는 주로 사용되는 메타 문자 몇 가지입니다.

- `.`: 모든 문자와 일치
- `*`: 이전 문자의 0회 이상의 반복과 일치
- `+`: 이전 문자의 1회 이상의 반복과 일치
- `?`: 이전 문자의 0회 또는 1회의 반복과 일치
- `^`: 문자열의 시작과 일치
- `$`: 문자열의 끝과 일치

자세한 사용 방법과 메타 문자 말고도 다양한 기능을 위해 추가적으로 알아야 할 것이 많습니다. 따라서 정규 표현식에 대한 심층적인 이해가 필요합니다.

## 또 다른 정보

아래는 정규 표현식에 대해 더 많이 알아볼 수 있는 링크입니다.

- [Haskell에 내장된 정규 표현식 패키지 문서](https://hackage.haskell.org/package/regex-base)
- [온라인 정규 표현식 테스트 사이트](https://regexr.com)
- [정규 표현식 강의 영상 (한글 번역)](https://youtu.be/mVgyKokKnVs)

# 참고 자료
---
title:                "정규 표현식 활용하기"
html_title:           "Arduino: 정규 표현식 활용하기"
simple_title:         "정규 표현식 활용하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이고, 왜 사용하는가?)

정규 표현식은 텍스트 패턴을 찾고 조작할 때 사용하는 강력한 도구입니다. 프로그래머는 코드를 더 깔끔하게 작성하고, 복잡한 문자열 처리 작업을 빠르게 수행할 수 있기 때문에 정규 표현식을 사용합니다.

## How to: (방법:)

Fish Shell에서 정규 표현식을 사용하는 예시를 보여드리겠습니다.

```Fish Shell
# 예시 1: 문자열이 'fish'로 시작하는지 확인
echo "fishing for compliments" | string match -r '^fish'

# 출력: fishing

# 예시 2: 파일 이름 목록에서 '.txt'로 끝나는 파일 찾기
string match -r '*.txt' (ls)

# 출력:
# document.txt
# notes.txt

# 예시 3: 문자열에서 숫자만 추출
echo "My number is 12345" | string match -r '[0-9]+'

# 출력: 12345
```

## Deep Dive (심층 분석):

정규 표현식은 1950년대 초반부터 수학자들 사이에서 사용되기 시작했습니다. POSIX와 Perl 호환 정규 표현식은 두 가지 주요한 표준입니다. Fish Shell은 기본적으로 Perl 호환 정규 표현식을 지원합니다. 그러나, Fish Shell은 string 명령을 이용하여 보다 명확하고 직관적인 문자열 처리를 제공하려고 노력합니다. 이는 문자열 작업을 위한 별도의 형식과 명령을 제공함으로써, 표준 Unix 도구들에 대한 의존성을 줄여줍니다.

## See Also (관련 자료):

- Fish Shell 공식 문서: [Regular Expressions in Fish](https://fishshell.com/docs/current/index.html#syntax-regular-expressions)
- 정규 표현식에 대한 더 깊은 이해를 위한 자료: [Regular-Expressions.info](https://www.regular-expressions.info/)
- 다른 프로그래밍 언어에서 정규 표현식 사용법: [MDN Web Docs 정규 표현식](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)

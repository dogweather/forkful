---
title:                "Fish Shell: 문자열 대문자로 변환하기"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜: 왜 문자열을 대문자로 바꾸어야 하는지에 대한 간단한 설명

문자열을 대문자로 바꾸는 것은 프로그래밍에서 자주 사용되는 작업입니다. 예를 들어 데이터베이스에서 가져온 사용자 이름이나, 사용자의 입력값을 처리할 때, 많은 경우에 대문자로 변환해야합니다. 이 작업을 통해 문자열을 비교하거나 필터링하는 것이 간단해지고, 보다 일관된 결과를 얻을 수 있습니다.

## Fish 쉘을 이용한 문자열 대문자로 변환하기

Fish Shell에서 문자열을 대문자로 변환하는 방법은 간단합니다. 먼저 문자열을 변수에 할당하고, 다음과 같이 `string toupper` 함수를 사용하면 됩니다.

```Fish Shell
set my_string "Hello, world!"
echo $my_string | string toupper
```

위의 코드를 실행하면 `HELLO, WORLD!`라는 결과가 출력됩니다. 또한, 내부에서 Fish Shell이 대문자로 자동 변환하는 `u` flag를 사용해도 같은 결과를 얻을 수 있습니다.

```Fish Shell
echo -u "Hello, world!"
```

다른 쉘 프로그램들과 마찬가지로 변수에 할당하여 사용하는 것이 가장 일반적인 방식입니다. 하지만 Fish Shell에는 내장되어 있는 문자열 처리 함수들을 사용할 수 있으므로, 편리하게 문자열을 대문자로 변환할 수 있습니다.

## 더 알아보기: 문자열 대문자로 변환하기

Fish 쉘에서 문자열을 대문자로 변환하는 것은 간단한 작업이지만, 내부에서 어떻게 처리되는지 더 알아볼 필요가 있습니다. Fish Shell에서는 다양한 문자열 처리 함수를 제공하며, `string toupper` 함수 역시 그 중 한 가지입니다. 이 함수는 주어진 문자열의 각 문자를 대문자로 변환하고, 새로운 문자열을 반환합니다. 또한, `string tolower` 함수를 사용하면 문자열을 소문자로 변환할 수도 있습니다.

Fish Shell에서는 기본적으로 Unicode를 지원합니다. 따라서, 문자열의 언어나 국적에 상관없이 모든 문자를 대문자로 변환할 수 있습니다. 또한, Fish Shell은 대소문자를 구분하지 않으므로 문자열을 비교할 때, 먼저 모두 대문자로 변환하고 이후에 비교하는 것이 좋습니다.

## 참고 자료

- [Fish Shell 공식 문서 (영어)](https://fishshell.com/docs/current/index.html)
- [Fish Shell GitHub 레포지토리 (영어)](https://github.com/fish-shell/fish-shell)
- [Fish Shell 커뮤니티 포럼 (영어)](https://github.com/fish-shell/fish-shell/)
- [Fish Shell 한국어 사용 설명서 (번역)](http://fishshell-dev.readthedocs.io/ko/latest/)
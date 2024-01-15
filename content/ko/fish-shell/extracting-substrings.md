---
title:                "서브스트링 추출 방법"
html_title:           "Fish Shell: 서브스트링 추출 방법"
simple_title:         "서브스트링 추출 방법"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

Substring 추출을 사용하는 이유는 변수 또는 문자열의 일부를 쉽게 추출하기 위해서입니다. 이를 통해 변수 또는 문자열을 더 용이하게 다룰 수 있고, 원하는 정보를 더 빠르고 정확하게 찾을 수 있습니다.

## 사용 방법

Fish Shell에서 substring을 추출하는 방법은 간단합니다. 예를 들어, 다음과 같은 문자열이 있다고 가정해 봅시다.

```
set food "sushi rolls"
```

이 문자열에서 "sushi"라는 부분만 추출하고 싶은 경우, 다음과 같이 입력해 줍니다.

```
set sushi (string match -r "sushi" $food)
```

이제 $sushi 변수에는 "sushi"라는 부분이 저장되어 있습니다. 즉, 변수를 이용해 부분문자열을 추출할 수 있습니다.

또 다른 예로, 다음과 같은 문자열이 있다고 가정해 봅시다.

```
set sentence "Hello, my name is John."
```

이 문자열에서 "John"이라는 이름만 추출하고 싶은 경우, 다음과 같이 입력해 줍니다.

```
set name (string replace "Hello, my name is " "" $sentence)
```

이제 $name 변수에는 "John"이라는 부분이 저장되어 있습니다. 즉, 문자열을 특정 패턴에 따라 치환하여 원하는 부분만 추출할 수 있습니다.

## 깊이 파고들기

Fish Shell에서 문자열 추출에 사용되는 명령어는 string match와 string replace 뿐만 아니라 string sub, string contains 등 다양합니다. 각각의 명령어는 자세한 옵션을 통해 다양한 방식으로 문자열을 추출하고 처리할 수 있도록 제공됩니다. 또한, 정규표현식을 사용하여 더 유연하게 문자열을 추출할 수도 있습니다.

이외에도 Fish Shell의 공식 문서나 온라인 커뮤니티에서 다양한 예제와 팁을 찾아보실 수 있습니다.

## 더 알아보기

- [Fish Shell 공식 페이지](https://fishshell.com/)
- [Fish Shell GitHub 레퍼지토리](https://github.com/fish-shell/fish-shell)
- [Fish Shell 커뮤니티 포럼](https://www.reddit.com/r/fishshell/)
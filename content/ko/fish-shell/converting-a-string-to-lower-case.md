---
title:                "Fish Shell: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 것이 중요한 이유는 데이터 처리 및 문자열 비교를 더 쉽고 간단하게 만들기 위해서입니다. 예를 들어, 입력된 문자열이 모두 소문자로 이루어졌는지 비교를 할 때, 입력된 문자열을 소문자로 변환하여 비교하는 것이 더 정확하고 간단한 방법입니다.

## 어떻게

```Fish Shell```을 사용하여 문자열을 소문자로 변환하는 방법은 매우 간단합니다. 먼저, 다음의 코드를 이용하여 원본 문자열을 변수에 할당합니다.

```
set original_string Hello World!
```

이제, ```string``` 명령어와 함께 ```lower``` 옵션을 사용하여 원본 문자열을 소문자로 변환합니다.

```
string lower $original_string
```

위의 코드를 실행하면 다음과 같은 결과가 출력됩니다.

```
hello world!
```

## 깊게 파고들기

Fish Shell의 ```string``` 명령어는 여러 가지 옵션을 제공합니다. ```lower``` 옵션 외에도, ```join```, ```length```, ```split``` 등 문자열을 다양한 방식으로 처리하는 옵션이 있습니다. 또한, 변수 뿐만 아니라 명령어의 출력 결과를 활용하여도 문자열을 소문자로 변환할 수 있습니다.

## 참고자료

- [Fish Shell 공식 홈페이지](https://fishshell.com/)
- [웹 개발을 위한 Fish Shell 튜토리얼](https://webinstall.dev/fish/)
- [Fish Shell을 활용한 스크립트 작성 방법](https://www.linode.com/docs/guides/linux-shell-scripting-with-fish/)
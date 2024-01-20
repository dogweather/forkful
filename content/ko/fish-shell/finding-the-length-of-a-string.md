---
title:                "문자열의 길이 찾기"
html_title:           "Lua: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열의 길이를 찾는 것은 문자열에 포함된 문자 수를 측정하는 작업입니다. 프로그래머가 이를 수행하는 이유는, 종종 데이터 검증, 문자열 조작이나 텍스트 데이터의 특정 부분에 액세스할 필요가 있기 때문입니다.

## 어떻게:

Fish shell에서 문자열의 길이를 찾는 가장 간단한 방법은 `string length`명령어를 사용하는 것입니다.

```fish
string length -q "Hello, World!"
```
위의 코드 실행 결과는 아래와 같습니다:

```fish
13
```
위 예시에서, "Hello, World!" 문자열의 길이는 13입니다.

## 딥 다이브:

Fish shell은 relatively new 프로그래밍 환경으로, 문자열 길이 계산과 같은 작업은 터미널 작업의 간결성과 효율성을 위해 제공됩니다. 다른 방식으로 `echo`와 `wc` 명령어를 복합적으로 사용하여 길이를 계산할 수도 있습니다. 예를 들면:

```fish
echo -n "Hello, World!" | wc -m
```

이 방식은 UNIX 스타일 가공을 선호하는 개발자에게 적합합니다. 하지만, 이것은 쉘에서 문자를 세는 표준적인 방법이 아니며, `wc` 명령어가 모든 시스템에서 동일한 방식으로 작동하지는 않을 수 있습니다.

## 참조 자료:

Fish shell에 대한 자세한 내용과 다른 명령어에 대한 정보는 다음 링크들을 참고하세요.
- Fish shell 공식 문서: https://fishshell.com/docs/current/index.html
- ‘wc’ 명령어에 대한 자세한 내용: https://www.geekhideout.com/wc.shtml
- 문자열 길이 계산에 대한 다른 접근 방식들: https://stackoverflow.com/questions/1568501/how-can-i-get-the-size-of-an-array-in-unix
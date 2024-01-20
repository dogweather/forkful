---
title:                "문자열 연결하기"
html_title:           "Arduino: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

문자열 연결이란 두 개 이상의 문자열을 하나로 합치는 프로그래밍 기법을 말합니다. 프로그래머들이 이를 사용하는 이유는 데이터 조작을 쉽고 빠르게 하기 위해서입니다.

## 어떻게 사용하는가:

Fish Shell에서 문자열을 연결하는 가장 간단한 방법은 아래의 예제들을 참고하세요.

```Fish Shell
set str1 "Fish"
set str2 "Shell"
set str $str1$str2
echo $str
```

위의 코드를 실행하면 아래의 결과를 볼 수 있습니다:

```Fish Shell
FishShell
```

## 심층 탐구

문자열 연결은 프로그래밍의 근본적인 개념 중 하나이며 이는 거의 모든 프로그래밍 언어에서 존재합니다. Fish Shell에서는 위에서 보여준 것처럼 간단하게 `$str1$str2`를 사용하여 문자열을 연결할 수 있습니다.

물론, Fish에서는 다른 방법으로도 문자열을 연결할 수 있습니다. 예를 들어, `string join` 명령을 사용하여 두 문자열을 연결할 수 있습니다:

```Fish Shell
set list (string split " " "Fish Shell")
string join -- "" $list
```

## 참고 자료

다음은 Fish Shell과 문자열 연결에 관한 추가 자료입니다:

1. [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)
2. [Fish 에서 string 조작에 대한 훌륭한 토론](https://stackoverflow.com/questions/27996706/string-manipulation-in-fish-shell)
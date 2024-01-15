---
title:                "문자열 연결하기"
html_title:           "Fish Shell: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

글자 연결을 하는 것에 대해 생각해 본 적이 있나요? Fish Shell은 여러 가지 쉬운 방법으로 문자열을 연결하는 것을 제공합니다. 이를 통해 코드를 더 간결하고 가독성 높게 유지할 수 있습니다.

## Fish Shell을 사용하여 문자열 연결하는 방법

먼저, Fish Shell에서는 "+" 기호를 사용하여 문자열을 연결할 수 있습니다. 예를 들어:

```Fish Shell
echo "Hello " + "World"
```

이 코드를 실행하면 "Hello World"라는 결과를 볼 수 있습니다. 또한, Fish Shell은 변수를 사용하여 더 복잡한 문자열을 만드는 것도 가능합니다. 예를 들어:

```Fish Shell
set greeting "Hello"
set name "John"
echo $greeting" "$name
```

이렇게 실행하면 "Hello John"이라는 결과를 볼 수 있습니다. 또 다른 방법으로는, "string replace" 함수를 사용하여 원하는 부분의 문자열을 변경하는 것도 가능합니다. 예를 들어:

```Fish Shell
set sentence "I love pie"
string replace love hate $sentence
```

이를 실행하면 "I hate pie"라는 결과를 볼 수 있습니다.

## 문자열 연결에 대해 깊이 파헤쳐보기

Fish Shell에서는 문자열 연결에 유용한 몇 가지 함수를 제공합니다. "string join" 함수는 배열의 요소들을 지정한 문자열로 연결할 수 있습니다. 예를 들어:

```Fish Shell
set names "John Bob Sarah"
string join ", " $names
```

이를 실행하면 "John, Bob, Sarah"라는 결과를 볼 수 있습니다. 또한, "string split" 함수를 사용하여 문자열을 배열로 분리할 수도 있습니다. 예를 들어:

```Fish Shell
set sentence "I love fish"
string split " " $sentence
```

이를 실행하면 ["I", "love", "fish"]라는 배열이 생성됩니다.

## See Also

- [Fish Shell 공식 사이트](https://fishshell.com/)
- [Fish Shell GitHub 저장소](https://github.com/fish-shell/fish-shell)
- [Fish Shell 문자열 함수 문서](https://fishshell.com/docs/current/cmds/string.html)
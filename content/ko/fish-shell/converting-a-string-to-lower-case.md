---
title:                "문자열 소문자로 변환하기"
html_title:           "Fish Shell: 문자열 소문자로 변환하기"
simple_title:         "문자열 소문자로 변환하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 것에 참여하는 이유는 무엇인가요? 이 글을 읽는다면 이미 이에 대해 궁금증이 생긴 것일 겁니다. 이를 위해서입니다! 일반적으로 소문자를 사용하여 코드를 작성하는 것은 더 읽기 쉽고 유지 관리가 쉽기 때문입니다.

## 해는

먼저, "```Fish Shell ... ```" 코드 블록에서 다음 예제를 사용하여 문자열을 소문자로 변환하는 방법을 알아보겠습니다.

```
Fish Shell> set string "HELLO WORLD"
Fish Shell> echo $string | tolower
hello world
```

위의 예제에서도 볼 수 있듯이, `tolower` 함수는 문자열을 소문자로 변환해주는 기능을 합니다. 만약 여러 개의 단어가 있는 문자열을 소문자로 변환하고 싶다면 `string split`과 `string join`을 사용하여 각 단어를 소문자로 변환한 뒤 다시 합칠 수 있습니다.

```
Fish Shell> set string "THIS IS A SENTENCE"
Fish Shell> set words (string split $string)
Fish Shell> for word in $words
               string join " " (string tolower $word)
           end
this is a sentence
```

이를 바탕으로, 여러분은 `tolower` 함수를 사용하여 원하는 문자열을 소문자로 변환할 수 있습니다.

## 딥 다이브

하지만 왜 `tolower`를 사용하면 문자열을 소문자로 변환할 수 있을까요? 이는 `tolower` 함수가 문자열의 모든 문자를 소문자로 변경하는 방법을 알고 있기 때문입니다. 이를 위해서 `string` 라이브러리 내의 `tolower` 함수를 알아야 합니다. 이 함수는 각 문자를 순회하고 `string tolower` 함수를 사용하여 각 문자를 소문자로 변환합니다.

```
function tolower
    set -l out ""
    for c in (string split '' $argv)
        set -l out $out(string tolower $c)
    end
    echo -I $out
end
```

더 깊이 들어가기 전에, `widestring`과 같은 유니코드 문자열이 있는 경우 `string tolower`를 사용하여 각 문자의 유니코드 포인트를 소문자로 변환해야 합니다. 이렇게 하면 모든 문자열이 소문자로 변환될 것입니다.

## 참고 자료

- [Fish Shell 공식 웹사이트](https://fishshell.com/)
- [Fish Shell 사용법 - YouTube 비디오](https://www.youtube.com/watch?v=5dFOOrP1yqI)
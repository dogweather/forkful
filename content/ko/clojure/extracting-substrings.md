---
title:    "Clojure: 문자열 추출하기"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜
: 부분 문자열 추출을 진행하는 이유에 대해 1-2 문장으로 설명합니다.

부분 문자열 추출은 다양한 프로그래밍 상황에서 유용하게 사용될 수 있습니다. 예를 들어, 문자열의 일부분만 필요한 경우나 문자열을 여러 개의 조각으로 나누어야 할 때 유용하게 쓰일 수 있습니다. 이를 통해 문자열을 더 쉽게 처리할 수 있고, 코드가 더 간결해질 수 있습니다.

## 사용 방법
: "```Clojure ... ```" 코드 블록 안에 코딩 예시와 예상 출력을 포함해 설명합니다.

```Clojure
; 문자열에서 부분 문자열 추출
(str/slice "Hello World" 0 5) ; "Hello"

; 인덱스 끝값을 생략하면 시작 인덱스부터 끝까지 추출됩니다.
(str/slice "Hello World" 6) ; "World"

; 음수 인덱스를 사용할 수도 있습니다. 뒤에서부터 인덱스를 세는 것을 의미합니다.
(str/slice "Hello World" -5) ; "World"

; 중간에 공백이 있는 문자열의 경우, 공백을 포함한 부분 문자열을 추출할 수도 있습니다.
(str/slice "Hello World" 6 11) ; "World"
```

## 깊게 들어가기
: 문자열 인덱싱과 관련된 정보를 더 자세히 설명합니다.

Clojure에서는 인덱스를 사용하여 문자열의 특정 부분을 쉽게 추출할 수 있습니다. 인덱스는 문자열의 각 문자에 할당된 숫자로, 첫 번째 문자는 0, 두 번째 문자는 1, 마지막 문자는 -1로 지정됩니다. 문자열에 인덱스를 사용하여 부분 문자열을 추출하면 시작 인덱스부터 끝 인덱스 직전까지의 문자열이 추출됩니다. 시작 인덱스와 끝 인덱스를 생략하면 문자열의 처음부터 끝까지 추출됩니다. 음수 인덱스를 사용할 경우, 뒤에서부터 인덱스를 세는 것을 의미합니다.

만약 문자열을 여러 개의 조각으로 나누어야 한다면, Clojure에서 제공하는 `subs` 함수를 사용하면 됩니다. `subs` 함수는 시작 인덱스부터 끝 인덱스 직전까지의 문자열을 추출해주는데, 추출한 문자열을 다시 `subs` 함수의 인자로 넣어줌으로써 여러 조각으로 나눌 수 있습니다.

## 참고
: 부분 문자열 추출과 관련된 유용한 정보를 포함하는 링크를 나열합니다.

- [Clojure string functions](https://gist.github.com/LangdalP/c6cbc363d318b5917d2d)
- [Substring extraction in Clojure](https://clojure.org/guides/strings#_string_functions)
- [ClojureDocs: str/slice](https://clojuredocs.org/clojure.string/slice)
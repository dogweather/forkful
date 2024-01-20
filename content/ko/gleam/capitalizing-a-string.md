---
title:                "문자열 대문자화"
html_title:           "Gleam: 문자열 대문자화"
simple_title:         "문자열 대문자화"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

- 문자열 대문자화는 모든 알파벳 문자를 대문자로 변경하는 것을 의미합니다.
- 프로그래머들은 사용자 입력 정규화, 시각적 표현 통일, 부분 검색 최적화 등의 목적으로 이 작업을 수행합니다.

## 어떻게 하는가:

아래 글림(Gleam) 코드를 통해 문자열 대문자화 코드 작성법을 확인합니다.  

```Gleam
import gleam/string

fn capitalize_string(input: String) -> String {
  string.capitalize(input)
}

fn main() {
  let my_string = "hello, world!"
  let result = capitalize_string(my_string)
  io.println(result)
}
```
예상 출력: "Hello, world!"

## 딥 다이브:

- 자연어 처리에서 문자열 대문자화는 오래전부터 널리 사용되었습니다. 이는 단어 대소문자 구분으로 인한 문제를 해결하기 위함입니다.
- Gleam 외의 다른 프로그래밍 언어에서도 대체 적으로 문자열 대문자화 기능이 지원됩니다. 예를 들어, Python의 `str.upper()`, JavaScript의 `String.prototype.toUpperCase()` 등이 있습니다.
- Gleam의 `string.capitalize`는 평균적으로 O(n)의 시간 복잡도를 갖는 구현입니다. 이 함수에는 모든 문자를 반복하며, 소문자일 경우 대문자로 변환하는 작업이 포함되어 있습니다.

## 참고자료:

다음은 문자열 대문자화에 대한 추가 정보에 도움이 될 수 있는 링크입니다.

2. "문자열 대문자화에 대한 팁": [https://stackoverflow.com/questions/17252232/what-is-the-best-way-to-convert-all-letters-to-uppercase](https://stackoverflow.com/questions/17252232/what-is-the-best-way-to-convert-all-letters-to-uppercase) (StackOverflow)
3. 유니코드 텍스트 정규화에 대한 "유니코드 표준 문서": [https://www.unicode.org/reports/tr15](https://www.unicode.org/reports/tr15) (Unicode Consortium)
---
title:                "패턴과 일치하는 문자 삭제하기"
html_title:           "Gleam: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 왜
왜 누군가 패턴에 맞는 문자를 삭제하는 것에 참여해야 할까요? 이 글에서는 그 이유를 알려드리도록 하겠습니다.

패턴에 맞는 문자를 삭제하는 작업은 데이터를 정리하고 선별하는 데 매우 유용합니다. 예를 들어, 사이트에서 이메일 주소나 전화번호처럼 정해진 패턴을 갖는 정보를 추출할 때 사용할 수 있습니다. 또한, 문장에서 특정한 단어나 문구를 삭제하고 싶을 때도 이 작업을 활용할 수 있습니다.

# 사용 방법
패턴에 맞는 문자를 삭제하기 위해 Gleam을 사용하는 방법은 간단합니다.

```Gleam
string = "Hello 123 World!"
processed_string = string |> Regex.replace(~r/\d+/, "")
```
위의 예시 코드에서는 바꾸고 싶은 문장이 `string` 변수에 저장되어 있고, 삭제할 문자의 패턴을 정규식으로 `~r/\d+/`과 같이 설정합니다. 그리고 나서 `Regex.replace` 함수를 사용하여 `processed_string` 변수에 저장하게 됩니다. 이제 `processed_string`을 출력하면 `Hello World!`가 출력될 것입니다.

# 심화 탐구
패턴에 맞는 문자를 삭제하는 방법은 정규식을 이용하면 더 강력하고 다양한 기능을 수행할 수 있습니다. Gleam에서는 `Regex` 라이브러리를 통해 다양한 정규식 패턴과 함수를 제공하기 때문에, 깊게 탐구하고 새로운 기능들을 익히는 것이 좋습니다.

# 더 알아보기
패턴에 맞는 문자를 삭제하는 데 유용한 정규식 패턴과 함수를 더 알아보고 싶다면 아래 링크들을 참고해보세요.

- [Gleam 공식 문서](https://gleam.run/modules/regex.html)
- [정규식 테스트 사이트](https://regex101.com/)
- [정규식 튜토리얼](https://regexone.com/)
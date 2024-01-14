---
title:                "Gleam: 서브스트링 추출하기"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

# 왜

문자열에서 부분 문자열을 추출하는 것이 왜 유용한지 궁금하신가요? 이번 블로그 포스트에서는 그 이유와 함께 Gleam 언어를 사용하여 문자열에서 부분 문자열을 추출하는 방법을 알려드리겠습니다.

## 하우 투

첫 번째 단계는 Gleam 언어를 이용해 추출하고자 하는 부분 문자열이 포함된 문자열을 정의하는 것입니다. 그 후에는 인덱스 값을 이용하여 원하는 부분 문자열을 추출할 수 있습니다. 아래는 이러한 과정을 보여주는 간단한 코드 예시와 실행 결과입니다.

```Gleam
let string = "안녕하세요, Gleam 언어입니다."
let substring = string[8..12]

//출력 결과: Gleam
```

위의 코드 예시에서는 문자열에서 인덱스 8부터 12까지의 값인 "Gleam" 부분 문자열을 추출하고 있습니다. 이와 같은 방법으로 원하는 부분 문자열을 추출할 수 있습니다.

## 딥 다이브

부분 문자열을 추출하는 것은 문자열 처리와 관련된 다양한 작업에서 유용하게 사용될 수 있습니다. 예를 들어, 데이터를 분석하거나 특정 단어를 검색하는 등의 작업에서 부분 문자열을 추출하여 더욱 효율적으로 작업할 수 있습니다. 또한, Gleam 언어에서는 다양한 문자열 처리 함수를 제공하기 때문에 부분 문자열을 추출하는 데에도 매우 유용합니다.

## See Also

- Gleam 언어 공식 문서: [https://gleam.run/documentation/](https://gleam.run/documentation/)
- Gleam 언어 개발자 포럼: [https://forum.gleam.run/](https://forum.gleam.run/)
- Gleam 언어 GitHub 저장소: [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)

이번 포스트에서 소개한 Gleam 언어의 부분 문자열 추출 기능 외에도 더욱 많은 기능과 라이브러리를 활용하여 다양한 작업을 수행할 수 있습니다. Gleam 언어를 사용하면 데이터 처리와 관련된 작업을 더욱 쉽고 효율적으로 수행할 수 있으니, 궁금하신 분들은 Gleam 언어 공식 문서나 개발자 포럼 등을 참고해보시기 바랍니다.
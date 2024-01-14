---
title:                "Gleam: 문자열을 소문자로 변환하기"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 왜
문자열을 소문자로 변환하는 것이 왜 중요한지 쉽게 이해할 수 있어야 합니다. 문자열을 소문자로 변환하면 데이터를 처리하기 쉽고 간편하게 만들어주기 때문입니다.

## 이렇게 하는 법
```Gleam
fn convert_to_lower(string: String) {
  string 
  |> String.to_lower_case 
  |> io.print_line
}

convert_to_lower("GLEAM") // output: gleam
```

위의 코드는 문자열을 소문자로 변환하는 간단한 예제입니다. 우선 함수를 선언하고, 문자열을 인자로 받아 문자열을 변환하고 출력하는 코드를 작성합니다. 그리고 `convert_to_lower` 함수를 호출하여 원하는 문자열을 소문자로 변환할 수 있습니다. 이렇게 함으로써 더 이상 문자열을 직접 소문자로 변환할 필요 없이 간단하게 함수를 호출하여 처리할 수 있습니다.

## 깊게 들어가기
문자열을 소문자로 변환하기 위해 Gleam에서 제공하는 내장 함수는 `String.to_lower_case` 입니다. 이 함수는 입력받은 문자열을 모두 소문자로 변환하여 반환해주는 역할을 합니다. 이 때, 문자열의 대소문자 구분을 무시하기 때문에 "GleAm", "gleam", "GLEAM" 모두 같은 결과값인 "gleam"을 반환합니다.

하지만 문자열의 대소문자를 구분해야 할 경우, `String.to_lower`와 `String.to_upper` 함수를 사용할 수 있으며 각각 소문자와 대문자로 변환해주는 함수입니다. 이를 활용하여 원하는 결과값을 얻을 수 있습니다.

# 관련 링크
- Gleam 공식 홈페이지: https://gleam.run/
- Gleam Github 저장소: https://github.com/gleam-lang/gleam
- Gleam 설치 가이드: https://gleam.run/getting-started/installation/
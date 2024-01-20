---
title:                "날짜를 문자열로 변환하기"
html_title:           "Arduino: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 뭔가요? 왜요?

날짜를 문자열로 변환하는 것은 사용자가 이해하기 쉬운 형식으로 날짜를 표시하는 프로그래밍 방법입니다. 프로그래머들이 이를 수행하는 주된 이유는 사용자 인터페이스에서 일관적이고 사용자 친화적인 날짜 표현을 제공하기 위해서입니다.

## 어떻게 해요?

아래의 Gleam 코드를 통해 날짜를 문자열로 변환하는 방법을 알아봅시다.

```gleam
import gleam/date.{Date, format}
import gleam/option.{unwrap}

fn main() {
  let today: Date = Date.new(2021, 12, 31)
  let today_string: Result(String, Nil) = format("{YYYY}-{MM}-{DD}", today)
  let result = unwrap(today_string)
    
  assert result == Ok("2021-12-31")
}
```

실행 결과로 "2021-12-31"가 출력됩니다. 

## 깊게 들여다보기

날짜를 문자열로 변환하는 기능은 아주 오래전부터 관례로 존재하였습니다. 이러한 변환은 날짜를 사람이 읽을 수 있는 형태로 표현하거나, 저장 공간을 절약하기 위해 이용됩니다.

다른 언어에서도 비슷한 기능을 찾을 수 있습니다. 예를 들어 JavaScript에서는 `toISOString` 함수, Python에서는 `strftime` 함수 등이 있습니다.

Gleam에서 날짜를 문자열로 변환하는 구현은 `gleam/date.format` 함수를 사용합니다. 이 함수는 날짜 형식을 결정하는 문자열과 선택한 날짜를 인수로 받아 문자열을 반환합니다.

```gleam
format(pattern: String, date: Date) -> Result(String, Nil)
```
반환된 결과는 문자열 또는 오류(Nil)입니다. 

## 참고资料

- Gleam 언어 Github: [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)
---
title:                "문자열에서 날짜 분석하기"
html_title:           "Gleam: 문자열에서 날짜 분석하기"
simple_title:         "문자열에서 날짜 분석하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열에서 날짜를 파싱한다는 것은, 문자열을 통해 날짜 정보를 추출하는 작업을 말합니다. 프로그래머들은 데이터 처리, 정렬, 비교 등을 위해 이와 같은 작업을 수행합니다.

## 어떻게 하나요:

Ruby에서 문자열에서 날짜를 파싱하기 위해 Date.parse 메소드를 사용할 수 있습니다. 먼저 'date' 라이브러리를 임포트해야 합니다.

```Ruby
require 'date'

date = Date.parse('2022-04-22')
puts date
```

결과는 다음과 같습니다:

```Ruby
2022-04-22
```

## 심화 학습:

1) 역사적 맥락: 초기 프로그래밍 언어에서는 문자열을 날짜로 변환하는 기능이 내장되지 않아 복잡한 알고리즘을 직접 구현해야 했습니다. Ruby에서는 'date' 라이브러리를 통해 이를 쉽게 할 수 있습니다.

2) 대체 방법: 구조화된 문자열에서 날짜를 파싱하는 것 외에, 시간 객체를 직접 생성하거나 epoch 시간(1970년 1월 1일 이후 초)을 계산하는 방법도 있습니다.

3) 구현 세부 사항: 내부적으로 Date.parse는 입력된 문자열을 분석하고 유효한 날짜 형식을 찾습니다. 유효한 형식이 없는 경우, ArgumentError가 발생합니다.

## 참고 자료:

1) Ruby 공식 문서 - Date.parse: https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html#method-c-parse
2) 문자열을 날짜로 변환하는 다양한 방법: https://www.justinweiss.com/articles/3-ways-to-parse-a-date-in-ruby/
3) Ruby에서 시간과 날짜 처리: https://www.tutorialspoint.com/ruby/ruby_date_time.htm
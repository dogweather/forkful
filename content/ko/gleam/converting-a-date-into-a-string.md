---
title:                "날짜를 문자열로 변환하기"
html_title:           "Gleam: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜
날짜를 문자열로 변환하는 일에 참여하는 이유를 최대 2문장으로 설명합니다.

데이터를 가독성 있게 표시하는 것이 중요할 뿐만 아니라 다른 언어와 통합할 때도 날짜를 문자열로 변환해야 할 수 있습니다.

## 사용 방법
"``` Gleam...
```" 코드 블록 내부에서 코딩 예제와 샘플 출력을 제공합니다.

``` Gleam
import gleam/date.format

let date = 2009-11-10T23:09:25Z

let string = Date.Format.to_string(date, "%m/%d/%Y")
```

출력:
``` Gleam
11/10/2009
```

## 자세히 알아보기
날짜를 문자열로 변환하기 위해 사용할 수 있는 다양한 형식을 알아봅니다. Gleam의 Date.Format 모듈을 사용하여 날짜와 형식을 입력하면 쉽게 문자열로 변환할 수 있습니다. 이 모듈을 사용하면 JSON 형식의 날짜를 날짜 객체로 바꾸는 것도 가능합니다.

## 관련 링크
- Gleam 공식 문서: https://gleam.run/documentation/standard-library/date
- 날짜 관련 Gleam 패키지: https://github.com/search?q=language%3AGleam+date&type=Repositories
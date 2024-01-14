---
title:                "Gleam: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

왜: 날짜를 문자열로 변환하는 것에 참여하는 이유를 간단히 설명하는 부분입니다.

날짜를 문자열로 변환하는 방법:

```Gleam
import Date.Format
import Gleam.Time

let date = Time.date(2021, 3, 20)
let format = "%Y-%m-%d"
let string_date = Date.Format.format(date, format)

// string_date는 "2021-03-20"을 반환합니다.
```

문자열 날짜로 변환하는 코드 예시와 출력 결과를 ```Gleam ...``` 코드 블록 안에서 제공합니다.

깊게 살펴보기:

날짜를 문자열로 변환하는 더 깊이있는 정보에 대해 다루는 부분입니다. 여러 가지 형식의 날짜를 문자열로 변환하는 방법이나 날짜 시간대 정보를 문자열로 포함시키는 방법 등 더 많은 세부 정보를 제공합니다.

참고자료:

## 외부 참고 자료
- [Gleam 문서 - 날짜 포맷팅](https://gleam.run/documentation/std/date_format)
- [Gleam 문서 - Date 패키지](https://gleam.run/documentation/std/date)

## 다른 프로그래밍 언어에서 날짜 변환하기
- [Python에서 날짜를 문자열로 변환하는 방법](https://www.programiz.com/python-programming/datetime/strftime)
- [JavaScript에서 날짜를 문자열로 변환하는 방법](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toString)
- [Java에서 날짜를 문자열로 변환하는 방법](https://www.baeldung.com/java-date-to-string)

See Also:
이제 이 글을 더 확장하여 다른 종류의 날짜 포맷팅을 Gleam에서 할 수 있는 방법에 대해 배웠습니다. 더 많은 정보를 찾기 위해 다음 링크를 참고하세요:

- [Gleam 문서 - 날짜 및 시간](https://gleam.run/documentation/std/time)
- [Gleam 문서 - String 모듈](https://gleam.run/documentation/std/string)
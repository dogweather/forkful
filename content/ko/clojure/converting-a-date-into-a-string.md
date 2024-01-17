---
title:                "날짜를 문자열로 변환하기"
html_title:           "Clojure: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
날짜를 문자열로 변환하는 것은 날짜 데이터를 사람이 이해할 수 있는 형식으로 표현하는 것을 의미합니다. 프로그래머들은 날짜 데이터를 다양한 출력 형식으로 변환할 필요가 있기 때문에 이 작업을 수행합니다.

## 방법:
`Clojure` 코드 블록 내에 코딩 예제와 출력 예시를 제공합니다.

```
; 날짜를 YYYY-MM-DD 형식의 문자열로 변환하기
(str (format "%04d-%02d-%02d" year month day))
; 결과: "2021-05-01"
```

```
; 날짜와 시간을 YYYY년 M월 D일 HH시 MM분 형식의 문자열로 변환하기
(str (format "%04d년 %d월 %d일 %02d시 %02d분" year month day hour minute))
; 결과: "2021년 5월 1일 09시 30분"
```

## 깊이 들어가기:
날짜를 문자열로 변환하는 작업은 다양한 프로그래밍 언어에서 지원되고 있으며, 대부분의 언어에서는 `format` 함수를 사용하거나 내장 함수를 제공합니다. `Clojure`에서는 `str` 함수와 `format` 함수를 사용하여 날짜를 원하는 형식의 문자열로 변환할 수 있습니다.

## 참고문헌:
- https://clojure.org/api/java.time/readme
- https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html